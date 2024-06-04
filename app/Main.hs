{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database (connectDb)
import Crud (Meal, getAllMeals, addMeal, updateMeal, deleteMeal, searchByName, filterByPrice)
import Commands (Command(..), parseCommand)
import Output (outToFile, outToHTML)
import Options.Applicative
import Data.Semigroup ((<>))
import Database.PostgreSQL.Simple (Connection)
import Logging (logCommand)
import Control.Monad (unless)

main :: IO ()
main = do
  command <- execParser opts
  result <- connectDb
  case result of
    Left err -> putStrLn err
    Right conn -> do
      logCommand command
      runCommand conn command
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> progDesc "Manage meals in the database"
     <> header "meals-cli - a CLI for managing meals" )

runCommand :: Connection -> Command -> IO ()
runCommand conn (AddCommand name price silent file) = do
  meal <- addMeal conn name price
  unless silent $ print meal
  outToFile file $ show meal

runCommand conn (UpdateCommand id name price isAvailable silent file) = do
  meal <- updateMeal conn id name price isAvailable
  unless silent $ print meal
  outToFile file $ show meal 

runCommand conn (DeleteCommand id silent) = do
  deleteMeal conn id
  unless silent $ putStrLn "Meal deleted successfully."

runCommand conn (ListCommand silent toHtml file) = do
  meals <- getAllMeals conn
  unless silent $ mapM_ print meals
  if toHtml then outToHTML . unlines $ map show meals else outToFile file $ unlines $ map show meals

runCommand conn (SearchCommand keyword silent file) = do
  meals <- getAllMeals conn
  let filteredMeals = searchByName keyword meals
  unless silent $ mapM_ print filteredMeals
  outToFile file $ unlines $ map show filteredMeals

runCommand conn (FilterByPriceCommand minPrice maxPrice silent file) = do
  meals <- getAllMeals conn
  let filteredMeals = filterByPrice minPrice maxPrice meals
  unless silent $ mapM_ print filteredMeals
  outToFile file $ unlines $ map show filteredMeals

