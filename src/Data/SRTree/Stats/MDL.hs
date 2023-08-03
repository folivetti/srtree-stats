{-# language FlexibleInstances #-}
module Data.SRTree.Stats.MDL ( aic, bic, mdl, mdlFreq, cl, cc, cp, replaceZeroTheta )
    where

import Data.List ( nub )
import Data.Maybe ( fromJust, fromMaybe )
import Control.Monad.Reader
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector as V
import Data.SRTree
import Data.SRTree.Print ( showExpr )
import Data.SRTree.Recursion ( cata, cataM ) 
import Data.SRTree.Opt
import Debug.Trace (trace)
import Control.Monad.State

type LogFun = Columns -> Column -> V.Vector Double -> Fix SRTree -> Double

-- | Bayesian information criterion
bic :: Distribution -> Maybe Double -> LogFun
bic d sErr x y theta t = (p + 1) * log n + 2 * nll d sErr x y t theta
  where
    p = fromIntegral $ V.length theta
    n = fromIntegral $ LA.size y

-- | Akaike information criterion
aic :: Distribution -> Maybe Double -> LogFun
aic d sErr x y theta t = 2 * (p + 1) + 2 * nll d sErr x y t theta
  where
    p = fromIntegral $ V.length theta
    n   = fromIntegral $ LA.size y

-- | Helper function to build an MDL function as the sum of different criteria
buildMDL :: [LogFun] -> LogFun
buildMDL fs x y theta t = foldr (\f acc -> acc + f x y theta t) 0 fs

-- | Helper function to invert the final arguments of nll to be compatible with LogFun (check if it is really needed)
nll' :: Distribution -> Maybe Double -> Columns -> Column -> V.Vector Double -> Fix SRTree -> Double
nll' d se x y theta t = nll d se x y t theta

-- | The negative log-likelihood (cl), log-functional structure (cc), log-parameters (cp)
cl, cp :: Distribution -> Maybe Double -> Columns -> Column -> V.Vector Double -> Fix SRTree -> Double
cc :: Maybe Double -> Columns -> Column -> V.Vector Double -> Fix SRTree -> Double
cl d sErr x y theta t = nll' d sErr x y theta t
cp d sErr x y theta t = logParameters d sErr x y theta t
cc sErr x y theta t = logFunctionalSimple x y theta t

-- | MDL as described in 
-- Bartlett, Deaglan J., Harry Desmond, and Pedro G. Ferreira. "Exhaustive symbolic regression." IEEE Transactions on Evolutionary Computation (2023).
mdl :: Distribution -> Maybe Double -> LogFun
mdl d sErr = buildMDL [nll' d sErr, logFunctionalSimple, logParameters d sErr]

-- | same as `mdl` but weighting the functional structure by frequency calculated using a wiki information of
-- physics and engineering functions
mdlFreq :: Distribution -> Maybe Double -> LogFun
mdlFreq d sErr = buildMDL [nll' d sErr, logFunctionalFreq, logParameters d sErr]

logFunctionalSimple :: LogFun
logFunctionalSimple _ _ _ t = (countNodes' t) * log (countUniqueTokens' t') + logC + (fromIntegral $ length consts) * log(2)
  where
    (t', _)            = floatConstsToParam t
    countNodes'        = fromIntegral . countNodes
    countUniqueTokens' = fromIntegral . length . nub . getOps
    consts             = getIntConsts t
    logC               = sum . map (log . abs) $ consts
    signs              = sum [1 | a <- getIntConsts t, a < 0] -- + sum [1 | a <- LA.toList theta, a < 0]

logFunctionalFreq  :: LogFun
logFunctionalFreq _ _ _ t = opToNat t' + logC + length' vars * (log (length' $ nub vars)) -- + (length' consts) * log(2)
  where
    (t', _)            = floatConstsToParam t
    consts             = getIntConsts t
    logC               = sum . map (log . abs) $ consts
    length'            = fromIntegral . length
    vars               = filter isVar $ getOps t
    isVar (VarToken _) = True
    isVar _            = False

logFunctionalFreqUniq  :: LogFun
logFunctionalFreqUniq _ _ _ t = opToNatUniq t + logC t
  where
    logC = sum . map (log . abs) . getIntConsts

logParameters :: Distribution -> Maybe Double -> LogFun
logParameters d msErr x y theta t = -(fromIntegral p / 2) * log 3 + sum logFisher + sum logTheta
  where
    p         = V.length theta
    logTheta  = V.toList $ V.map (log . abs) theta
    logFisher = map ((* 0.5) . log) $ fisherNLL d msErr x y t theta -- is (*0.5) dependent of guassian?
{-
fisherInfo :: Distribution -> Maybe Double -> Columns -> Column -> V.Vector Double -> Fix SRTree -> [Double]
fisherInfo msErr x y theta t = do 
  ix <- [0 .. p-1]
  let f'      = deriveBy True ix t'
      f''     = deriveBy True ix f'
      fvals'  = evalTree x theta LA.scalar f'
      fvals'' = evalTree x theta LA.scalar f''
      f_ii    = LA.toList $ fvals' ^ 2 - res*fvals''
  pure $ sum f_ii / (sErr ^ 2)
  where
    p    = V.length theta
    (t', _)   = floatConstsToParam t
    res  = y - evalTree x theta LA.scalar t'
    ssr  = sse x y t theta
    sErr = case msErr of
             Nothing -> sqrt $ ssr / fromIntegral (LA.size y - p)
             Just x  -> x
-}
countUniqueTokens :: Fix SRTree -> Int
countUniqueTokens t = countFuns t + countOp t
  where
      countFuns = length . nub . getFuns
      countOp   = length . nub . getOps

getIntConsts :: Fix SRTree -> [Double]
getIntConsts = cata alg
  where
    alg (Uni f t) = t
    alg (Bin op l r) = l <> r
    alg (Var ix) = []
    alg (Param _) = []
    alg (Const x) = [x | fromIntegral (round x) == x]

data CountToken = OpToken Op | FunToken Function | VarToken Int | ConstToken | ParamToken
    deriving (Show, Eq, Ord)

getOps :: Fix SRTree -> [CountToken]
getOps = cata alg
  where
    alg (Uni f t) = FunToken f : t
    alg (Bin op l r) = OpToken op : (l <> r)
    alg (Var ix) = [VarToken ix]
    alg (Param _) = [ParamToken]
    alg (Const _) = [ConstToken]

getFuns :: Fix SRTree -> [Function]
getFuns = cata alg
  where
    alg (Uni f t) = f : t
    alg (Bin _ l r) = l <> r
    alg _ = []

opToNat :: Fix SRTree -> Double
opToNat = cata alg
  where
    alg (Uni f t) = funToNat f + t
    alg (Bin op l r) = opToNat' op + l + r
    alg _ = 0.6610799229372109

opToNat' :: Op -> Double
opToNat' Add = 2.500842464597881
opToNat' Sub = 2.500842464597881
opToNat' Mul = 1.720356134912558
opToNat' Div = 2.60436883851265
opToNat' Power = 2.527957363394847

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

opToNatUniq :: Fix SRTree -> Double
opToNatUniq = cata alg
  where
    alg (Uni f t) = funToNatUniq f + t
    alg (Bin op l r) = opToNatUniq' op + l + r
    alg _ = 0.49920052443651175

opToNatUniq' :: Op -> Double
opToNatUniq' Add = 2.706526197759235
opToNatUniq' Sub = 2.706526197759235
opToNatUniq' Mul = 2.2113039088452564
opToNatUniq' Div = 2.723161258263767
opToNatUniq' Power = 2.592699087013154

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

replaceZeroTheta :: Distribution -> Maybe Double -> Columns -> Column -> Fix SRTree -> Fix SRTree
replaceZeroTheta d msErr x y t = t''
  where
    fisher = fisherNLL d msErr x y t (V.fromList theta)
    (t', theta)     = floatConstsToParam t
    info = zip theta fisher
    t''    = go t'

    go :: Fix SRTree -> Fix SRTree
    go = cata alg
      where
        alg (Var ix)  = Fix (Var ix)
        alg (Const c) =  Fix (Const c)
        alg (Param ix) = let (v, f) = info !! ix
                             v' = if abs (v / sqrt(12 / f) ) < 1 then 0 else v
                          in Fix (Const v')
        alg (Uni f t) = Fix (Uni f t)
        alg (Bin op l r) = case op of
                            Add -> l + r
                            Sub -> l - r
                            Mul -> l * r
                            Div -> l / r
                            Power -> l ** r
