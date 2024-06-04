{-# LANGUAGE OverloadedStrings #-}

module Output (outToFile, outToHTML) where

import Commands (Command(..), FlagParameter(..))
import Control.Monad(unless)    
import System.IO (writeFile)

outToFile :: Maybe FilePath -> String -> IO ()
outToFile Nothing _ = pure () 
outToFile (Just path) content = writeFile path content

outToHTML :: String -> IO ()
outToHTML content = do
    let htmlContent = "<html><head><title>HTML Output</title></head><body><p>" ++ content ++ "</p></body></html>" 
    writeFile "index.html" htmlContent

