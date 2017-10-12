allNats :: [Int] -> Bool
allNats xs = all (> 0) xs

pairUp :: [a] -> [(a, a)]
pairUp []       = []
pairUp [x]   = []
pairUp (x:y:xs) = [(x, y)] ++ pairUp xs

data CountTree a = Empty | Node a (CountTree a) (CountTree a) deriving (Show)

height :: CountTree a -> Int

insert :: Float -> CountTree Float -> CountTree Float



4.
i. c
ii. b
iii. d
iv. d