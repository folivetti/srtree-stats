{-# language LambdaCase #-}
{-# language ImportQualifiedPost #-}
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as B
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.SRTree (SRTree, countNodes)
import Data.SRTree.EqSat (simplifyEqSat)
import Data.SRTree.Opt
import Data.SRTree.Stats.MDL (aic, bic, mdl, mdlFreq)
import Numeric.LinearAlgebra qualified as LA
import Options.Applicative
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, stdout)
import Text.ParseSR (SRAlgs (..))
import Text.ParseSR.IO (withInput)
import Text.Read (readMaybe)

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

data Args = Args
    {   from        :: SRAlgs
      , infile      :: String
      , outfile     :: String
      , dataset     :: String
      , test        :: String
      , trainRows   :: Int
      , cols        :: [Int]
      , target      :: Int
      , test_target :: Int
      , niter       :: Int
      , hasHeader   :: Bool
      , simpl       :: Bool
      , ms2         :: Maybe Double
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
       ( long "dataset"
       <> short 'd'
       <> metavar "DATASET"
       <> showDefault
       <> value ""
       <> help "Filename of the dataset used for optimizing the parameters. Empty string omits stats that make use of the training data." )
   <*> strOption
       ( long "test"
       <> metavar "TEST"
       <> showDefault
       <> value ""
       <> help "Filename of the test dataset. Empty string omits stats that make use of the training data." )
   <*> option auto
       ( long "rows"
       <> short 'r'
       <> metavar "ROWS"
       <> showDefault
       <> value 0
       <> help "Number of rows to use as training data, the remainder will be used as validation. Values <= 0 will use the whole data as training and validation.")
   <*> option columnsReader
       ( long "columns"
       <> short 'c'
       <> metavar "COLUMNS"
       <> showDefault
       <> value []
       <> help "Index of columns to use as variables. Default \"\" uses all but the last column.")
   <*> option auto
       ( long "target"
       <> short 't'
       <> metavar "TARGET"
       <> showDefault
       <> value (-1)
       <> help "Index of column to use as the target variable. Default (-1) uses the last column.")
   <*> option auto
       ( long "test-target"
       <> metavar "TEST-TARGET"
       <> showDefault
       <> value (-1)
       <> help "Index of column to use as the target variable for the test set. Default (-1) uses the last column.")
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
       ( long "s2"
       <> metavar "S2"
       <> showDefault
       <> value Nothing
       <> help "Estimated s^2 of the data. If not passed, it uses the model MSE.")

openData :: Args -> IO (((Columns, Column), (Columns, Column)), [(B.ByteString, Int)])
openData args = first (splitTrainVal (trainRows args)) 
             <$> loadDataset (dataset args) (cols args) (target args) (hasHeader args)

printResults :: String -> String -> (SRTree Int Double -> String) -> [Either String (SRTree Int Double)] -> IO ()
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
  mTrain <- if (null $ dataset args) then pure Nothing else Just <$> openData args
  mTest <- if (null (dataset args) || null (test args)) then pure Nothing else Just <$> loadDataset (test args) (cols args) (test_target args) (hasHeader args)
  let Just (((xTr, yTr),(xVal, yVal)), headers) = mTrain
      Just ((xTe, yTe), _) = mTest
      optimizer     = optimize (niter args) xTr yTr
      varnames      = mUnless (null $ dataset args) $ intercalate "," (map (B.unpack.fst) headers)
      header_csv = "number_nodes,number_params" 
                 <> mUnless (null $ dataset args) ",sse_train,sse_val,mse_train,mse_val,bic,aic,mdl,mdl_freq" 
                 <> mUnless (null (dataset args) || null (test args)) ",sse_test,mse_test"
      genStats tree = let tree' = if simpl args then simplifyEqSat tree else tree
                          t     = if niter args == 0 || null (dataset args) then tree' else optimizer tree'
                          theta = getTheta t
                          s2    = ms2 args
                          stats = [fromIntegral . countNodes, fromIntegral . const (LA.size theta)]
                                <> mUnless (null $ dataset args) 
                                       [sse xTr yTr, sse xVal yVal, mse xTr yTr
                                       , mse xVal yVal, bic s2 xTr yTr theta
                                       , aic s2 xTr yTr theta, mdl s2 xTr yTr theta
                                       , mdlFreq s2 xTr yTr theta]
                                <> mUnless (null (dataset args) || null (test args)) 
                                       [sse xTe yTe, mse xTe yTe]
                        in intercalate "," $ map (show . ($ t)) stats
  withInput (infile args) (from args) varnames False False
    >>= printResults (outfile args) header_csv genStats
  
  where 
      opts = info (opt <**> helper)
            ( fullDesc <> progDesc "Optimize the parameters of Symbolic Regression expressions."
            <> header "srtree-opt - a CLI tool to (re)optimize the numeric parameters of symbolic regression expressions"
            )
