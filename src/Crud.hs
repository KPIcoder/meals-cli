{-# LANGUAGE OverloadedStrings #-}

module Crud (
  Meal(..),
  getAllMeals,
  addMeal,
  updateMeal,
  deleteMeal,
  searchByName,
  filterByPrice
) where

import Data.List (isInfixOf)
import Data.Char (toLower)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Meal = Meal {
  mealId :: Int,
  mealName :: String,
  mealPrice :: Int,
  mealIsAvailable :: Bool
} deriving Show

instance FromRow Meal where
  fromRow = Meal <$> field <*> field <*> field <*> field

getAllMeals :: Connection -> IO [Meal]
getAllMeals conn = query_ conn "SELECT id, name, price, is_available FROM meal"

addMeal :: Connection -> String -> Int -> IO Meal
addMeal conn name price = do
  let isAvailable = True
  [meal] <- query conn "INSERT INTO meal (name, price, is_available) VALUES (?, ?, ?) Returning *" (name, price, isAvailable)
  return meal

updateMeal :: Connection -> Int -> String -> Int -> Bool -> IO Meal
updateMeal conn id name price isAvailable = do
  [meal] <- query conn "UPDATE meal SET name = ?, price = ?, is_available = ? WHERE id = ? Returning *" (name, price, isAvailable, id)
  return meal

deleteMeal :: Connection -> Int -> IO ()
deleteMeal conn id = do
  execute conn "DELETE FROM meal WHERE id = ?" (Only id)
  return ()

searchByName :: String -> [Meal] -> [Meal]
searchByName keyword meals = filter (\meal -> isInfixOf (map toLower keyword) (map toLower $ mealName meal)) meals

filterByPrice :: Int -> Int -> [Meal] -> [Meal]
filterByPrice minPrice maxPrice meals = filter (\meal -> mealPrice meal >= minPrice && mealPrice meal <= maxPrice) meals
