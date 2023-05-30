{-# language FlexibleInstances #-}
module Data.SRTree.Stats.MDL ( aic, bic, mdl, mdlFreq, replaceZeroTheta )
    where

import Data.List ( nub )
import Data.Maybe ( fromJust )
import Control.Monad.Reader
import qualified Numeric.LinearAlgebra as LA
import Data.SRTree
import Data.SRTree.Opt
import Debug.Trace (trace)
import Control.Monad.State

type LogFun = Columns -> Column -> Column -> SRTree Int Double -> Double

bic :: Maybe Double -> LogFun
bic sErr x y theta t = (p + 1) * log n + 2 * negLogLikelihood sErr x y theta t
  where
    p = fromIntegral $ LA.size theta
    n = fromIntegral $ LA.size y

aic :: Maybe Double -> LogFun
aic sErr x y theta t = 2 * (p + 1) + 2 * negLogLikelihood sErr x y theta t
  where
    p = fromIntegral $ LA.size theta
    n   = fromIntegral $ LA.size y

buildMDL :: [LogFun] -> LogFun
buildMDL fs x y theta t = foldr (\f acc -> acc + f x y theta t) 0 fs

mdl :: Maybe Double -> LogFun
mdl sErr = buildMDL [negLogLikelihood sErr, logFunctionalSimple, logParameters sErr]

mdlFreq :: Maybe Double -> LogFun
mdlFreq sErr = buildMDL [negLogLikelihood sErr, logFunctionalFreq, logParameters sErr]

negLogLikelihood :: Maybe Double -> LogFun
negLogLikelihood msErr x y theta t = 0.5*ssr/(sErr*sErr) + 0.5*m*log(2*pi*sErr*sErr)
  where
    m   = fromIntegral $ LA.size y
    n   = fromIntegral $ LA.size theta
    ssr = sse x y t
    sErr = case msErr of
           Nothing -> sqrt $ ssr / (m - n)
           Just x  -> x

logFunctionalSimple :: LogFun
logFunctionalSimple _ _ theta t = (countNodes' t) * log (countUniqueTokens' t') + logC + (fromIntegral $ length consts) * log(2)
  where
    t'                 = constToParam t
    countNodes'        = fromIntegral . countNodes
    countUniqueTokens' = fromIntegral . length . nub . getOps
    consts             = getIntConsts t
    logC               = sum . map (log . abs) $ consts
    signs = sum [1 | a <- getIntConsts t, a < 0] -- + sum [1 | a <- LA.toList theta, a < 0]

logFunctionalFreq  :: LogFun
logFunctionalFreq _ _ _ t = opToNat t' + logC + length' vars * (log (length' $ nub vars)) -- + (length' consts) * log(2)
  where
    t'                 = constToParam t
    consts             = getIntConsts t
    logC               = sum . map (log . abs) $ consts
    length' = fromIntegral . length
    vars = filter isVar $ getOps t
    isVar (VarOp _) = True
    isVar _         = False

logFunctionalFreqUniq  :: LogFun
logFunctionalFreqUniq _ _ _ t = opToNatUniq t + logC t
  where
    logC = sum . map (log . abs) . getIntConsts

logParameters :: Maybe Double -> LogFun
logParameters msErr x y theta t = -(fromIntegral p / 2) * log 3 + sum logFisher + sum logTheta
  where
    p         = LA.size theta
    logTheta  = LA.toList . log . abs $ theta
    logFisher = map ((* 0.5) . log) $ fisherInfo msErr x y theta t

fisherInfo :: Maybe Double -> Columns -> Column -> Column -> SRTree Int Double -> [Double]
fisherInfo msErr x y theta t = do 
  ix <- [0 .. p-1]
  let f'      = deriveBy ix t'
      f''     = deriveBy ix f'
      fvals'  = evalSRTree theta f'
      fvals'' = evalSRTree theta f''
      f_ii    = LA.toList $ fvals' ^ 2 - res*fvals''
  pure $ sum f_ii / (sErr ^ 2)
  where
    p    = LA.size theta
    t'   = paramToVar $ varToConst x $ constToParam t
    res  = y - evalSRTree theta t'
    ssr  = sse x y t
    sErr = case msErr of
           Nothing -> sqrt $ ssr / fromIntegral (LA.size y - p)
           Just x  -> x

evalSRTree :: Column -> SRTree Int Column -> Column
evalSRTree theta tree = fromJust $ evalTree tree `runReader` (Just . LA.scalar . (theta LA.!))

data Op = PowOp | AddOp | SubOp | MulOp | DivOp | PowerOp | LogOp | VarOp Int | ConstOp | ParamOp | FunOp Function
    deriving (Show, Eq, Ord)

countUniqueTokens :: SRTree Int a -> Int
countUniqueTokens t = countFuns t + countOp t
  where
      countFuns = length . nub . getFuns
      countOp   = length . nub . getOps

getIntConsts :: SRTree Int Double -> [Double]
getIntConsts (Pow node i)  = fromIntegral i : getIntConsts node
getIntConsts (Const x)     = [x | fromIntegral (round x) == x]
getIntConsts (Fun _ node)  = getIntConsts node
getIntConsts (Add l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Sub l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Mul l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Div l r)     = getIntConsts l <> getIntConsts r
getIntConsts (Power l r)   = getIntConsts l <> getIntConsts r
getIntConsts (LogBase l r) = getIntConsts l <> getIntConsts r
getIntConsts _             = []

getOps :: SRTree Int val -> [Op]
getOps (Fun f node)  = FunOp f : getOps node
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
opToNat (Param _)     = 0.6610799229372109
opToNat (Const _)     = 0.6610799229372109
opToNat (Mul l r)     = 1.720356134912558 + opToNat l + opToNat r
opToNat (Div l r)     = 2.60436883851265 + opToNat l + opToNat r
opToNat (Add l r)     = 2.500842464597881 + opToNat l + opToNat r
opToNat (Sub l r)     = 2.500842464597881 + opToNat l + opToNat r
opToNat (Power l r)   = 2.527957363394847 + opToNat l + opToNat r
opToNat (Pow l _)     = 2.527957363394847 + opToNat l + 0.6610799229372109
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

replaceZeroTheta :: Maybe Double -> Columns -> Column -> Column -> SRTree Int Double -> SRTree Int Double
replaceZeroTheta msErr x y theta t = trace (show fisher) $ simplify t''
  where
    fisher = fisherInfo msErr x y theta t
    t'     = constToParam t
    theta' = LA.toList theta
    t'' = go t' `evalState` (zip theta' fisher)

    go :: SRTree Int Double -> State [(Double, Double)] (SRTree Int Double)
    go Empty         = pure Empty
    go (Var ix)      = pure $ Var ix
    go (Param _)     = do (v, f) <- gets head
                          modify tail
                          let v' = if abs (v / sqrt(12 / f) ) < 1 then 0 else v
                          pure $ Const v'
    go (Const v)     = pure $ Const v
    go (Fun f t)     = Fun f <$> go t
    go (Pow t i)     = (`Pow` i) <$> go t
    go (Add l r)     = do { l' <- go l; r' <- go r; pure $ Add l' r' }
    go (Sub l r)     = do { l' <- go l; r' <- go r; pure $ Sub l' r' }
    go (Mul l r)     = do { l' <- go l; r' <- go r; pure $ Mul l' r' }
    go (Div l r)     = do { l' <- go l; r' <- go r; pure $ Div l' r' }
    go (Power l r)   = do { l' <- go l; r' <- go r; pure $ Power l' r' }
    go (LogBase l r) = do { l' <- go l; r' <- go r; pure $ LogBase l' r' }
