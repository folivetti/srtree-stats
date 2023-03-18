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
      , niter       :: Int
      , hasHeader   :: Bool
      , simpl       :: Bool
      , gz          :: Bool
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
       <> help "Filename of the dataset used for optimizing the parameters." )
   <*> strOption
       ( long "test"
       <> metavar "TEST"
       <> help "Filename of the test dataset." )
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
       <> help "Index of colum to use as the target variable. Default (-1) uses the last column.")
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
    <*> switch
        ( long "gz"
        <> help "Gzipped files.")
   <*> option s2Reader
       ( long "s2"
       <> metavar "S2"
       <> showDefault
       <> value Nothing
       <> help "Estimated s^2 of the data. If not passed, it uses the model MSE.")

openData :: Args -> IO (((Columns, Column), (Columns, Column)), [(B.ByteString, Int)])
openData args = first (splitTrainVal (trainRows args)) 
             <$> loadDataset (dataset args) (cols args) (target args) (hasHeader args) (gz args)

printResults :: String -> (SRTree Int Double -> String) -> [Either String (SRTree Int Double)] -> IO ()
printResults fname f exprs = do
  h <- if null fname then pure stdout else openFile fname WriteMode
  hPutStrLn h "number_nodes,number_params,sse_train,sse_val,sse_test,mse_train,mse_val,mse_test,bic,aic,mdl,mdl_freq"
  forM_ exprs $ \case 
                   Left  err -> hPutStrLn h $ "invalid expression: " <> err
                   Right ex  -> hPutStrLn h $ f ex
                   -- Right ex  -> putStrLn (SRP.showDefault ex) >> (hPutStrLn h $ f ex)
  unless (null fname) $ hClose h

main :: IO ()
main = do
  args <- execParser opts
  (((xTr, yTr),(xVal, yVal)), headers) <- openData args
  ((xTe, yTe), _) <- loadDataset (test args) (cols args) (target args) (hasHeader args) (gz args)
  let optimizer     = optimize (niter args) xTr yTr
      varnames      = intercalate "," (map (B.unpack.fst) headers)
      genStats tree = let tree' = if simpl args then simplifyEqSat tree else tree
                          t     = optimizer tree'
                          n     = countNodes t
                          theta = getTheta t
                          p     = LA.size theta
                          s2    = ms2 args
                          sses  = map show [sse xTr yTr t, sse xVal yVal t, sse xTe yTe t]
                          mses  = map show [mse xTr yTr t, mse xVal yVal t, mse xTe yTe t]
                          cmplx = map show [bic s2 t xTr yTr theta, aic s2 t xTr yTr theta, mdl s2 t xTr yTr theta, mdlFreq s2 t xTr yTr theta]
                        in intercalate "," $ [show n, show p] <> sses <> mses <> cmplx
  withInput (infile args) (from args) varnames False False
    >>= printResults (outfile args) genStats
  
  where 
      opts = info (opt <**> helper)
            ( fullDesc <> progDesc "Optimize the parameters of Symbolic Regression expressions."
            <> header "srtree-opt - a CLI tool to (re)optimize the numeric parameters of symbolic regression expressions"
            )
