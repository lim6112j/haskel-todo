{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Yaml as Yaml
import GHC.Generics
import Options.Applicative hiding (infoParser)
import System.IO.Error
import Data.Time
data Command =
  Info
  | Init
  | List
  | Add Item
  | View ItemIndex
  | Update ItemIndex ItemUpdate
  | Remove ItemIndex
  deriving Show
type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe String
type ItemDueBy = Maybe LocalTime

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

data Options = Options FilePath Command deriving Show
data Item = Item
  { title :: ItemTitle
  , description :: ItemDescription
  , priority :: ItemPriority
  , dueBy :: ItemDueBy
  } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item
data ItemUpdate = ItemUpdate
  { titleUpdate :: Maybe ItemTitle
  , descriptionUpdate :: Maybe ItemDescription
  , priorityUpdate :: Maybe ItemPriority
  , dueByUpdate :: Maybe ItemDueBy
  } deriving Show
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
itemTitleValueParser :: Parser String
itemTitleValueParser =
  strOption (long "title" <> short 't' <> metavar "TITLE" <> help "Title")
itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption (long "description" <> short 'd' <> metavar "DESCRITION" <> help "Description")
itemPriorityValueParser :: Parser String
itemPriorityValueParser =
  strOption (long "Priority" <> short 'p' <> metavar "PRIORITY" <> help "Priority")
--itemDueByValueParser :: Parser String
--itemDueByValueParser =
--  strOption (long "DueBy" <> short 'b' <> metavar "DUEBY" <> help "Due By")
itemDueByValueParser :: Parser LocalTime
itemDueByValueParser =
  option readDateTime (long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by")
  where
    readDateTime = eitherReader $ \arg ->
      case parseDateTimeMaybe arg of
        (Just dateTime) -> Right dateTime
        nothing -> Left $ "Date/time string must be in " ++ dateTimeFormat ++ " format"
    parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
    dateTimeFormat = "%Y/%m/%d %H:%M:%S"
infoParser :: Parser Command
infoParser = pure Info
initParser :: Parser Command
initParser = pure Init
listParser :: Parser Command
listParser = pure List
addParser :: Parser Command
addParser = Add <$> addItemParser
viewParser :: Parser Command
viewParser = View <$> itemIndexParser
updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser
removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser
commandParser :: Parser Command
commandParser = subparser $ mconcat
  [
    command "info" (info infoParser (progDesc "show info"))
   ,command "init" (info initParser (progDesc "show init"))
   ,command "list" (info listParser (progDesc "show list"))
   ,command "add" (info addParser (progDesc "show add"))
   ,command "view" (info viewParser (progDesc "show view"))
   ,command "update" (info updateParser (progDesc "show update"))
   ,command "remove" (info removeParser (progDesc "show remove"))
  ]

optionsParser :: Parser Options
optionsParser = Options <$> dataPathParser <*> commandParser
addItemParser :: Parser Item
addItemParser = Item
  <$> argument str (metavar "TITLE" <> help "title")
  <*> optional itemDescriptionValueParser
  <*> optional itemPriorityValueParser
  <*> optional itemDueByValueParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
  <$> optional updateItemTitleParser
  <*> optional updateItemDescriptionParser
  <*> optional updateItemPriorityParser
  <*> optional updateItemDueByParser
updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser
updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-description")
updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
  Just <$> itemPriorityValueParser
  <|> flag' Nothing (long "clear-priority")
updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser =
  Just <$> itemDueByValueParser
  <|> flag' Nothing (long "clear-due-by")

main :: IO ()
main = do
  Options dataPath command <- execParser (info (optionsParser) (progDesc "To-Do List Manager"))
  let dueBy = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)
  writeToDoList dataPath $ ToDoList
    [ Item "title1" (Just "description1") (Just "priority1") (Just dueBy)
    , Item "title2" (Just "description2") (Just "priority2") (Just dueBy)
    ]
  toDoList <- readToDoList dataPath
  print toDoList
--main = do
--  todoList <- readToDoList "file.txt"
--  print todoList
--main = do
--  Options dataPath command  <- execParser (info (optionsParser) (progDesc "Todo list Manager"))
--  Run dataPath command
run :: FilePath -> Command -> IO()
run dataPath Info = putStrLn "Info"
run dataPath Init = putStrLn "Init"
run dataPath List = putStrLn "List"
run dataPath (Add item)   = putStrLn $ "Add: item = " ++ show item
run dataPath (View itemIndex)  = putStrLn $ "View: itemIndex = " ++ show itemIndex
run dataPath (Update idx itemUpdate) = putStrLn $ "Update: idx= " ++ show idx ++ " itemUpdate = " ++ show itemUpdate
run dataPath (Remove itemIndex) = putStrLn $ "Remove: itemIndex = " ++ show itemIndex
 -- dataPath <- execParser (info (dataPathParser) (progDesc "ToDo list file path"))
  --putStrLn $ "dataPath = " ++ show dataPath
  --itemIndex <- execParser (info (itemIndexParser) (progDesc "To do list program"))
  --putStrLn $ "itemindex =" ++ show itemIndex

writeToDoList :: FilePath -> ToDoList -> IO()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList) 
--readToDoList :: FilePath -> IO(Maybe ToDoList)
--readToDoList dataPath = BS.readFile dataPath >>= return . Yaml.decode
readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
  mbToDoList <- catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (BS.readFile dataPath >>= return . Yaml.decode)
    (\_ -> return $ Just (ToDoList []))
  case mbToDoList of
    Nothing -> error "YAML file is corrut"
    Just toDoList -> return toDoList
