{-# LANGUAGE OverloadedStrings #-}

module Logging (logCommand) where

import System.IO
import Data.Time.Clock
import Data.Time.Format

import Commands (Command)

logCommand :: Command -> IO ()
logCommand command = do
    currentTime <- getCurrentTime
    let timeStamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    let logEntry = timeStamp ++ " - " ++ show command ++ "\n"
    appendFile "logs.txt" logEntry
