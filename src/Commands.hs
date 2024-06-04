{-# LANGUAGE OverloadedStrings #-}

module Commands (
  Command(..),
  FlagParameter(..),
  parseCommand
) where

import Options.Applicative
import Data.Semigroup ((<>))

type FlagParameter = Bool

data Command
  = AddCommand String Int FlagParameter (Maybe FilePath)
  | UpdateCommand Int String Int Bool FlagParameter (Maybe FilePath)
  | DeleteCommand Int FlagParameter 
  | ListCommand FlagParameter FlagParameter (Maybe FilePath)
  | SearchCommand String FlagParameter (Maybe FilePath)
  | FilterByPriceCommand Int Int FlagParameter (Maybe FilePath)
  deriving Show

parseAdd :: Parser Command
parseAdd = AddCommand
  <$> strArgument (metavar "NAME" <> help "Name of the meal")
  <*> argument auto (metavar "PRICE" <> help "Price of the meal")
  <*> switch (long "silent" <> short 's' <> help "Silent mode")
  <*> optional (strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "File output path"))

parseUpdate :: Parser Command
parseUpdate = UpdateCommand
  <$> argument auto (metavar "ID" <> help "ID of the meal")
  <*> strArgument (metavar "NAME" <> help "Name of the meal")
  <*> argument auto (metavar "PRICE" <> help "Price of the meal")
  <*> switch (long "available" <> short 'a' <> help "Specify if the meal is available")
  <*> switch (long "silent" <> short 's' <> help "Silent mode")
  <*> optional (strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "File output path"))

parseDelete :: Parser Command
parseDelete = DeleteCommand
  <$> argument auto (metavar "ID" <> help "ID of the meal")
  <*> switch (long "silent" <> short 's' <> help "Silent mode")

parseList :: Parser Command
parseList = ListCommand
  <$> switch (long "silent" <> short 's' <> help "Silent mode")
  <*> switch (long "html" <> short 'H' <> help "HTML view")
  <*> optional (strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "File output path"))

parseSearch :: Parser Command
parseSearch = SearchCommand
  <$> strArgument (metavar "KEYWORD" <> help "Keyword to search for in meal names")
  <*> switch (long "silent" <> short 's' <> help "Silent mode")
  <*> optional (strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "File output path"))

parseFilterByPrice :: Parser Command
parseFilterByPrice = FilterByPriceCommand
  <$> argument auto (metavar "MIN_PRICE" <> help "Minimum price")
  <*> argument auto (metavar "MAX_PRICE" <> help "Maximum price")
  <*> switch (long "silent" <> short 's' <> help "Silent mode")
  <*> optional (strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "File output path"))



parseCommand :: Parser Command
parseCommand = subparser
  ( command "add" (info parseAdd (progDesc "Add a new meal"))
 <> command "update" (info parseUpdate (progDesc "Update an existing meal"))
 <> command "delete" (info parseDelete (progDesc "Delete a meal"))
 <> command "list" (info parseList (progDesc "List all meals"))
 <> command "search" (info parseSearch (progDesc "Search for meals by keyword"))
 <> command "filter" (info parseFilterByPrice (progDesc "Filter meals by price range"))
  )
