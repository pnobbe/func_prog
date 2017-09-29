{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = a :> [Rose a]
    deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (a :> _) = a

children :: Rose a -> [Rose a]
children (_ :> rs) = rs

-- Exercise 2

size :: Rose a -> Int
size (_ :> rs) = 1 + sum [ size x | x <- rs ]

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> rs) = sum [ leaves x | x <- rs ]

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"
    
-- Exercise 3
    
nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((a1, a2, a3),(b1, b2, b3),(c1, c2, c3)) = ((a1, b1, c1),(a2, b2, c2),(a3, b3, c3))

diagonals :: Board -> (Row, Row)
diagonals ((a1, _, c2),(_, b, _),(c1, _, a2)) = ((a1, b, a2), (c2, b, c1))

-- Exercise 6

emptyBoard :: Board
emptyBoard = (row, row, row)
        where row = (B,B,B)

-- Exercise 7

printBoard :: Board -> String
printBoard (a, b, c) = printRow a ++ line ++ printRow b ++ line ++ printRow c
          where line               = "-+-+-\n"
                printRow (d, e, f) = show d ++ "|" ++ show e ++ "|" ++ show f ++ "\n"


-- | Move generation

boardToList :: Board -> [Field]
boardToList ((a,b,c), (d,e,f), (g,h,i)) = [a,b,c,d,e,f,g,h,i]

listToBoard :: [Field] -> Board
listToBoard [a,b,c,d,e,f,g,h,i] = ((a,b,c), (d,e,f), (g,h,i))

-- Exercise 8

moves :: Player -> Board -> [Board]
moves p b = map listToBoard $ catMaybes [boardSet i | i <- [0..8]]
                where (new, lboard) = (symbol p, boardToList b)
                      boardSet i = case splitAt i lboard of
                                        (start, B:end) -> Just (start ++ [new] ++ end)
                                        _              -> Nothing


-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner b | ((X,X,X),_,_) <- b            = Just P1
            | (_,(X,X,X),_) <- b            = Just P1
            | (_,_,(X,X,X)) <- b            = Just P1
            | ((X,X,X),_,_) <- verticals b  = Just P1
            | (_,(X,X,X),_) <- verticals b  = Just P1
            | (_,_,(X,X,X)) <- verticals b  = Just P1
            | ((X,X,X),_)   <- diagonals b  = Just P1
            | (_,(X,X,X))   <- diagonals b  = Just P1
            | ((O,O,O),_,_) <- b            = Just P2
            | (_,(O,O,O),_) <- b            = Just P2
            | (_,_,(O,O,O)) <- b            = Just P2
            | ((O,O,O),_,_) <- verticals b  = Just P2
            | (_,(O,O,O),_) <- verticals b  = Just P2
            | (_,_,(O,O,O)) <- verticals b  = Just P2
            | ((O,O,O),_)   <- diagonals b  = Just P2
            | (_,(O,O,O))   <- diagonals b  = Just P2
            | otherwise                     = Nothing



-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree p b
        | hasWinner b <= Nothing = b :> [ gameTree (nextPlayer p) board | board <- moves p b ]
        | otherwise              = b :> []

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves $ gameTree P1 ((B,B,B),(B,B,B),(B,B,B))

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax p (r :> [])
                | hasWinner r == Just p              = 1    :> []
                | hasWinner r == Just (nextPlayer p) = (-1) :> []
                | otherwise                          = 0    :> []

minimax p (_ :> rs)     = maximum' [root x | x <- xs] :> xs
           where xs     = [ minimax' (nextPlayer p) b | b <- rs ]


minimax' p (r :> [])
                | hasWinner r == Just p              = (-1) :> []
                | hasWinner r == Just (nextPlayer p) = 1   :> []
                | otherwise                          = 0  :> []

minimax' p (_ :> rs)    = minimum' [root x | x <- xs] :> xs
           where xs     = [ minimax (nextPlayer p) b | b <- rs ]


-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' []  = error "empty list"
minimum' [x] = x
minimum' (x:y:xs)
    | x   == -1 = -1
    | otherwise = minimum' ((if x < y then x else y):xs)

maximum' :: [Int] -> Int
maximum' []  = error "empty list"
maximum' [x] = x
maximum' (x:y:xs)
    | x == 1    = 1
    | otherwise = maximum' ((if x > y then x else y):xs)


-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove p b
        | isJust (hasWinner b)   = Nothing
        | isNothing index  = Nothing
        | otherwise = Just $ root (children (gameTree p b) !! fromJust index )
          where findOptimal  = minimax p (gameTree p b)
                index = root findOptimal `elemIndex` map root (children findOptimal)



-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType 
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b = do
            let possibleMoves = moves p b
            if null possibleMoves then
                return Nothing
            else do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map (lines . (\ (i, b) -> "(" ++ show i ++ "): \n" ++ printBoard b))
                    . zip [1 :: Integer ..]


    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y
