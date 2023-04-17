{-# language FlexibleInstances #-}
module Data.SRTree.Stats.MDL ( aic, bic, mdl, mdlFreq )
    where

import Data.List ( nub )
import Data.Maybe ( fromJust )
import Control.Monad.Reader
import qualified Numeric.LinearAlgebra as LA
import Data.SRTree
import Data.SRTree.Opt
import Debug.Trace (trace)

type LogFun = Columns -> Column -> Column -> SRTree Int Double -> Double

bic :: Maybe Double -> LogFun
bic s2 x y theta t = p * log n + 2 * negLogLikelihood s2 x y theta t
  where
    p = fromIntegral $ LA.size theta
    n = fromIntegral $ LA.size y

aic :: Maybe Double -> LogFun
aic s2 x y theta t = 2 * p + 2 * negLogLikelihood s2 x y theta t
  where
    p = fromIntegral $ LA.size theta
    n   = fromIntegral $ LA.size y

buildMDL :: [LogFun] -> LogFun
buildMDL fs x y theta t = foldr (\f acc -> acc + f x y theta t) 0 fs

mdl :: Maybe Double -> LogFun
mdl s2 = buildMDL [negLogLikelihood s2, logFunctionalSimple, logParameters]

mdlFreq :: Maybe Double -> LogFun
mdlFreq s2 = buildMDL [negLogLikelihood s2, logFunctionalFreq, logParameters]

negLogLikelihood :: Maybe Double -> LogFun
negLogLikelihood ms2 x y theta t = 0.5*n*log(2*pi*s2) + 0.5*ssr/s2-- 0.5*n*(log(2*pi) + log(ssr/n) + 1)
  where
    m   = fromIntegral $ LA.size y
    n   = fromIntegral $ LA.size theta
    ssr = sse x y t
    s2 = case ms2 of
           Nothing -> sqrt $ ssr / (m - n)
           Just x  -> x

logFunctionalSimple :: LogFun
logFunctionalSimple _ _ _ t = countNodes' t * log (countUniqueTokens' t) + logC t
  where
    countNodes'        = fromIntegral . countNodes
    countUniqueTokens' = fromIntegral . countUniqueTokens
    logC               = sum . map log . getIntConsts

logFunctionalFreq  :: LogFun
logFunctionalFreq _ _ _ t = opToNat t + logC t
  where
    logC = sum . map log . getIntConsts

logFunctionalFreqUniq  :: LogFun
logFunctionalFreqUniq _ _ _ t = opToNatUniq t + logC t
  where
    logC = sum . map log . getIntConsts

logParameters :: LogFun
logParameters x y theta t = -(fromIntegral p / 2) * log 3 + sum logFisher + sum logTheta
  where
    p         = LA.size theta
    t'        = paramToVar $ varToConst x $ constToParam t
    res       = y - evalSRTree theta t'
    logTheta  = LA.toList . log . abs $ theta
    logFisher = map ( (* 0.5) . log) fisher
    s2        = mse x y t
    fisher    = do ix <- [0 .. p-1]
                   let f'      = deriveBy ix t'
                       f''     = deriveBy ix f'
                       fvals'  = evalSRTree theta f'
                       fvals'' = evalSRTree theta f''
                       f_ii    = LA.toList $ fvals'*fvals' + res*fvals''
                   pure $ sum f_ii -- / s2

evalSRTree :: Column -> SRTree Int Column -> Column
evalSRTree theta tree = fromJust $ evalTree tree `runReader` (Just . LA.scalar . (theta LA.!))

data Op = PowOp | AddOp | SubOp | MulOp | DivOp | PowerOp | LogOp | VarOp Int | ConstOp | ParamOp
    deriving (Show, Eq, Ord)

countUniqueTokens :: SRTree Int Double -> Int
countUniqueTokens t = countFuns t + countOp t
  where
      countFuns = length . nub . getFuns
      countOp   = length . nub . getOps
      
getIntConsts :: SRTree ix val -> [Double]
getIntConsts (Pow node i)  = fromIntegral i : getIntConsts node
getIntConsts (Fun _ node)  = getIntConsts node
getIntConsts (Add l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Sub l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Mul l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Div l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Power l r)   = getIntConsts l <> getIntConsts r
getIntConsts (LogBase l r) = getIntConsts l <> getIntConsts r
getIntConsts _             = []

getOps :: SRTree Int val -> [Op]
getOps (Fun _ node)  = getOps node
getOps (Pow node _)  = PowOp : getOps node
getOps (Add l r)     = AddOp : (getOps l <> getOps r)
getOps (Sub l r)     = SubOp : (getOps l <> getOps r)
getOps (Mul l r)     = MulOp : (getOps l <> getOps r)
getOps (Div l r)     = DivOp : (getOps l <> getOps r)
getOps (Power l r)   = PowerOp : (getOps l <> getOps r)
getOps (LogBase l r) = LogOp : (getOps l <> getOps r)
getOps (Var ix)       = [VarOp ix]
getOps (Const _)     = [ConstOp]
getOps (Param _)     = [ParamOp]
getOps Empty         = []

getFuns :: SRTree ix val -> [Function]
getFuns (Fun f node)  = f : getFuns node
getFuns (Pow node _)  = getFuns node
getFuns (Add l r)     = getFuns l <> getFuns r
getFuns (Sub l r)     = getFuns l <> getFuns r
getFuns (Mul l r)     = getFuns l <> getFuns r
getFuns (Div l r)     = getFuns l <> getFuns r
getFuns (Power l r)   = getFuns l <> getFuns r
getFuns (LogBase l r) = getFuns l <> getFuns r
getFuns _             = []

opToNat :: SRTree ix val -> Double
opToNat (Var _)       = 0.6610799229372109
opToNat (Mul l r)     = 1.720356134912558 + opToNat l + opToNat r
opToNat (Div l r)     = 2.60436883851265 + opToNat l + opToNat r
opToNat (Add l r)     = 2.500842464597881 + opToNat l + opToNat r
opToNat (Sub l r)     = 2.500842464597881 + opToNat l + opToNat r
opToNat (Power l r)   = 2.527957363394847 + opToNat l + opToNat r
opToNat (Pow l _)     = 2.527957363394847 + opToNat l
opToNat (LogBase l r) = 4.765599813200964 + opToNat l + opToNat r
opToNat (Fun f l)     = funToNat f + opToNat l
opToNat _             = 0

funToNat :: Function -> Double
funToNat Sqrt = 4.780867285331753
funToNat Log  = 4.765599813200964
funToNat Exp  = 4.788589331425663
funToNat Abs  = 6.352564869783006
funToNat Sin  = 5.9848400896576885
funToNat Cos  = 5.474014465891698
funToNat Sinh = 8.038963823353235
funToNat Cosh = 8.262107374667444
funToNat Tanh = 7.85664226655928
funToNat Tan  = 8.262107374667444
funToNat _    = 8.262107374667444
--funToNat Factorial = 7.702491586732021

opToNatUniq :: SRTree ix val -> Double
opToNatUniq (Var _)       = 0.49920052443651175
opToNatUniq (Mul l r)     = 2.2113039088452564 + opToNatUniq l + opToNatUniq r
opToNatUniq (Div l r)     = 2.723161258263767 + opToNatUniq l + opToNatUniq r
opToNatUniq (Add l r)     = 2.706526197759235 + opToNatUniq l + opToNatUniq r
opToNatUniq (Sub l r)     = 2.706526197759235 + opToNatUniq l + opToNatUniq r
opToNatUniq (Power l r)   = 2.592699087013154 + opToNatUniq l + opToNatUniq r
opToNatUniq (Pow l _)     = 2.592699087013154 + opToNatUniq l
opToNatUniq (LogBase l r) = 4.5885325448350684 + opToNatUniq l + opToNatUniq r
opToNatUniq (Fun f l)     = funToNatUniq f + opToNatUniq l
opToNatUniq _             = 0

funToNatUniq :: Function -> Double
funToNatUniq Sqrt = 4.887387917884975
funToNatUniq Log  = 4.5885325448350684
funToNatUniq Exp  = 4.62243409651075
funToNatUniq Abs  = 6.157148462748913
funToNatUniq Sin  = 5.738438127890729
funToNatUniq Cos  = 5.484203989506487
funToNatUniq Sinh = 7.766586375183014
funToNatUniq Cosh = 7.9897299264972235
funToNatUniq Tanh = 7.584264818389059
funToNatUniq Tan  = 7.9897299264972235
funToNatUniq _    = 7.9897299264972235
--funToNatUniq Factorial = 7.584264818389059
