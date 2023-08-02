{-# language LambdaCase #-}
{-# language ImportQualifiedPost #-}
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as B
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.SRTree (SRTree, Fix, countNodes, floatConstsToParam, paramsToConst)
import Data.SRTree.EqSat (simplifyEqSat)
import Data.SRTree.Opt hiding (loadDataset)
import Data.SRTree.Stats.MDL (aic, bic, mdl, mdlFreq, cl, cc, cp, replaceZeroTheta)
import Data.SRTree.Datasets (loadDataset)
import Numeric.LinearAlgebra qualified as LA
import Data.Vector qualified as V
import Options.Applicative
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, stdout)
import Text.ParseSR (SRAlgs (..))
import Text.ParseSR.IO (withInput)
import Text.Read (readMaybe)
import Debug.Trace ( trace )
import qualified Data.SRTree.Print as SRP

envelope :: a -> [a] -> [a]
envelope c xs = c : xs <> [c]

sralgsHelp :: [String]
sralgsHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: SRAlgs ..]

sralgsReader :: ReadM SRAlgs
sralgsReader = do
  sr <- map toUpper <$> str
  eitherReader $ case readMaybe sr of
    Nothing -> pure . Left $ "unknown algorithm. Available options are " <> intercalate "," sralgsHelp
    Just x  -> pure . Right $ x

columnsReader :: ReadM [Int]
columnsReader = do
  colsStr <- ('[':) . (<> "]") <$> str
  eitherReader $ case readMaybe colsStr of
    Nothing -> pure . Left $ "wrong format " <> colsStr
    Just x  -> pure . Right $ x

s2Reader :: ReadM (Maybe Double)
s2Reader = do
  s <- str
  eitherReader $ case readMaybe s of
    Nothing -> pure . Left $ "wrong format " <> s
    mx      -> pure . Right $ mx

distHelp :: [String]                                                                                          
distHelp = map (envelope '\'' . map toLower . show) [toEnum 0 :: Distribution ..]                             
{-# INLINE distHelp #-}

distRead :: ReadM (Maybe Distribution)                                                                        
distRead = do
    d <- capitalize <$> str
    eitherReader $ case readMaybe d of
                     Nothing -> pure . Left $ "unsupported distribution " <> d
                     Just x  -> pure . Right $ Just x
    where
      capitalize ""     = ""
      capitalize (c:cs) = toUpper c : map toLower cs

data Args = Args
    {   from        :: SRAlgs
      , infile      :: String
      , outfile     :: String
      , dataset     :: String
      , test        :: String
      , niter       :: Int
      , hasHeader   :: Bool
      , simpl       :: Bool
      , msErr       :: Maybe Double
      , dist        :: Maybe Distribution
    } deriving Show

opt :: Parser Args
opt = Args
   <$> option sralgsReader
       ( long "from"
       <> short 'f'
       <> metavar ("[" <> intercalate "|" sralgsHelp <> "]")
       <> help "Input expression format" )
   <*> strOption
       ( long "input"
       <> short 'i'
       <> metavar "INPUT"
       <> showDefault
       <> value ""
       <> help "Input file containing expressions. Empty string gets expression from stdin." )
   <*> strOption
       ( long "output"
       <> short 'o'
       <> metavar "OUTPUT"
       <> showDefault
       <> value ""
       <> help "Output file to store stats for each expression. Empty string prints expressions to stdout." )
   <*> strOption
       ( long "training"
       <> metavar "DATASET"
       <> showDefault
       <> value ""
       <> help "Filename of the dataset used for optimizing the parameters. Empty string omits stats that make use of the training data. It will auto-detect and handle gzipped file based on gz extension. It will also auto-detect the delimiter. \nThe filename can include extra information: filename.csv:start:end:target:vars where start and end corresponds to the range of rows that should be used for fitting, target is the column index (or name) of the target variable and cols is a comma separated list of column indeces or names of the variables in the same order as used by the symbolic model." )
   <*> strOption
       ( long "test"
       <> metavar "TEST"
       <> showDefault
       <> value ""
       <> help "Filename of the test dataset. Empty string omits stats that make use of the training data. It can have additional information as in the training set, but the validation range will be discarded." )
   <*> option auto
       ( long "niter"
       <> metavar "NITER"
       <> showDefault
       <> value 10
       <> help "Number of iterations for the optimization algorithm.")
   <*> switch
       ( long "hasheader"
       <> help "Uses the first row of the csv file as header.")
    <*> switch
        ( long "simplify"
        <> help "Apply EqSat simplification." )
   <*> option s2Reader
       ( long "sErr"
       <> metavar "Serr"
       <> showDefault
       <> value Nothing
       <> help "Estimated standard error of the data. If not passed, it uses the model MSE.")
   <*> option distRead
       ( long "distribution"
       <> metavar ("[" <> intercalate "|" distHelp <> "]")                                                   
       <> showDefault                                                                                        
       <> value Nothing                                                                                      
       <> help "Minimize negative log-likelihood following one of the avaliable distributions. The default will use least squares to optimize the model."
       ) 

printResults :: String -> String -> (Fix SRTree -> String) -> [Either String (Fix SRTree)] -> IO ()
printResults fname header f exprs = do
  h <- if null fname then pure stdout else openFile fname WriteMode
  hPutStrLn h header
  forM_ exprs $ \case 
                   Left  err -> hPutStrLn h $ "invalid expression: " <> err
                   Right ex  -> hPutStrLn h $ f ex
                   -- Right ex  -> putStrLn (SRP.showDefault ex) >> (hPutStrLn h $ f ex)
  unless (null fname) $ hClose h

mWhen :: Monoid m => Bool -> m -> m
mWhen False m = mempty
mWhen True m = m
{-# inline mWhen #-}

mUnless :: Monoid m => Bool -> m -> m
mUnless False m = m
mUnless True m = mempty
{-# inline mUnless #-}

main :: IO ()
main = do
  args <- execParser opts
  mTrain <- if null (dataset args) 
               then pure Nothing 
               else Just <$> loadDataset (dataset args) (hasHeader args)
  mTest <- if null (dataset args) || null (test args) 
              then pure Nothing 
              else Just <$> loadDataset (test args) (hasHeader args)
  let Just ((xTr, yTr, xVal, yVal), headers) = mTrain
      Just ((xTe, yTe, _, _), _) = mTest
      myDist = case dist args of
                 Nothing -> Gaussian
                 Just x  -> x
      optimizer     = optimize (Just myDist) (msErr args) (niter args) xTr yTr Nothing
      varnames      = mUnless (null $ dataset args) headers
      header_csv = "number_nodes,number_params" 
                 <> mUnless (null $ dataset args) ",sse_train,sse_val,mse_train,mse_val,bic,aic,mdl,mdl_freq,cl,cc,cp" 
                 <> mUnless (null (dataset args) || null (test args)) ",sse_test,mse_test"
      genStats tree = let t' = if null (dataset args) 
                                then tree
                                else if niter args == 0
                                       then tree -- replaceZeroTheta sErr xTr yTr tree
                                       else let (tt, th, _) = optimizer tree in paramsToConst (V.toList th) tt -- replaceZeroTheta sErr xTr yTr (fst $ optimizer tree)
                          (t, theta') = trace (SRP.showExpr t') $ floatConstsToParam t'
                          theta = V.fromList theta'
                          sErr  = msErr args
                          sse' x y t = sse x y t theta
                          mse' x y t = mse x y t theta
                          
                          stats = [fromIntegral . countNodes, fromIntegral . const (V.length theta)]
                                <> mUnless (null $ dataset args) 
                                       [ sse' xTr yTr, sse' xVal yVal
                                       , mse' xTr yTr , mse' xVal yVal
                                       , bic myDist sErr xTr yTr theta , aic myDist sErr xTr yTr theta
                                       , mdl myDist sErr xTr yTr theta , mdlFreq myDist sErr xTr yTr theta
                                       , cl myDist sErr xTr yTr theta
                                       , cc sErr xTr yTr theta
                                       , cp sErr xTr yTr theta
                                       ]
                                <> mUnless (null (dataset args) || null (test args)) 
                                       [sse' xTe yTe, mse' xTe yTe]
                        in intercalate "," $ map (show . ($ t')) stats
  withInput (infile args) (from args) varnames False (simpl args)
    >>= printResults (outfile args) header_csv genStats
  
  where 
      opts = info (opt <**> helper)
            ( fullDesc <> progDesc "Optimize the parameters of Symbolic Regression expressions."
            <> header "srtree-opt - a CLI tool to (re)optimize the numeric parameters of symbolic regression expressions"
            )
