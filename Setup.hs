module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Control.Arrow

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (lines >>> exercise >>> unlines)

exercise :: [String] -> [String]
exercise = parseTable >>> select "gender" "male"
                      >>> project ["last", "first", "salary"] >>> printTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable s
  | null s = return []
  | otherwise = map $ words s


-- | Printing

-- * Exercise 2

printLine :: [Int] -> String
printLine xs
  | null xs = return []
  | otherwise = putStrLn . concat $ (map (\i -> ["+"] ++ replicate i "-") xs) ++ ["+"]

-- * Exercise 3

printField :: Int -> String -> String
printField = undefined

-- * Exercise 4
               
printRow :: [(Int, String)] -> String
printRow = undefined

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths = undefined

-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows)
    = undefined

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    = undefined

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    = undefined