{-# LANGUAGE OverloadedStrings #-}

module Database (connectDb) where

import Database.PostgreSQL.Simple
import Control.Exception (try, SomeException)

connectDb :: IO (Either String Connection)
connectDb = do
  let connInfo = defaultConnectInfo {
                    connectHost = "localhost",
                    connectPort = 5432,
                    connectUser = "postgres",
                    connectPassword = "Qwerty12345",
                    connectDatabase = "postgres"
                  }
  conn <- try (connect connInfo) :: IO (Either SomeException Connection)
  case conn of
    Left err  -> return $ Left $ "Connection failed: " ++ show err
    Right con -> return $ Right con
