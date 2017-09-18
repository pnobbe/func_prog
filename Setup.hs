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
exercise = parseTable >>> select "gender" "male" >>> project ["last", "first", "salary"] >>> printTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable s | null s    = []
             | otherwise = map words s


-- | Printing

-- * Exercise 2

printLine :: [Int] -> String -- Opinions?
printLine xs | null xs   = []
             | otherwise = concat $ "+" : [ replicate x '-' ++ "+" | x <- xs ]

-- * Exercise 3

printField :: Int -> String -> String
printField n s | all isDigit s = spaces ++ s
               | otherwise               = s ++ spaces
               where spaces = replicate (n - length s) ' '

-- * Exercise 4
               
printRow :: [(Int, String)] -> String -- Feel like the implementation 'looks messy'
printRow []             = []
printRow xss@((_, _):_) = "|" ++ intercalate "|" (map (\(n, s) -> uncurry printField (n, s)) xss) ++ "|"

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths t = [ maximum $ map length x | x <- transpose t ]

-- * Exercise 6

printTable :: Table -> [String] -- Could probably be done better, but how?
printTable []                  = []
printTable table@(header:rows) =
    [ line, printRow $ zip maxWidths [map toUpper x | x <- header], line] ++ -- Formatted header
    [ printRow n | n <- map (zip maxWidths) rows] ++ [line] -- Formatted rows
    where maxWidths  = columnWidths table
          line       = printLine maxWidths

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table -- Is this a good implementation of optional values?
select column value table@(header:rows)
    | isNothing hIndex = table
    | isJust hIndex    = header : [ x | x <- rows, x !! fromJust hIndex == value ]
    where hIndex = elemIndex column header


-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    = transpose [ t !! i | i <- indices]
    where t       = transpose table
          indices = mapMaybe (`elemIndex` header) columns