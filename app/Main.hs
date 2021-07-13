module Main where

import Options.Applicative

type ItemIndex = Int
data Options = Options FilePath ItemIndex deriving Show

defaultDataPath :: FilePath
defaultDataPath = "./to-do.yaml"
dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath
  <> long "data-path"
  <> short 'p'
  <> metavar "FILEPATH"
  <> help ("path to data file (default " ++ defaultDataPath ++ ")")
itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")
optionsParser :: Parser Options
optionsParser = Options <$> dataPathParser <*> itemIndexParser
main :: IO ()
main = do
  options <- execParser (info (optionsParser) (progDesc "Todo list Manager"))
  putStrLn $ "options = " ++ show options
 -- dataPath <- execParser (info (dataPathParser) (progDesc "ToDo list file path"))
  --putStrLn $ "dataPath = " ++ show dataPath
  --itemIndex <- execParser (info (itemIndexParser) (progDesc "To do list program"))
  --putStrLn $ "itemindex =" ++ show itemIndex

