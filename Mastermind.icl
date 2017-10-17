module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -> Int
positionalMatches [] _ = 0
positionalMatches _ [] = 0
positionalMatches [x:xs] [y:ys]
	| x == y    = 1 + positionalMatches xs ys
	| otherwise = 0 + positionalMatches xs ys

matches :: [Int] [Int] -> Int
matches [] _ = 0
matches _ [] = 0
matches [x:xs] ys = minLength [x:xs] ys + matches (filter ((<>)x) xs) (filter ((<>)x) ys)
	where
		minLength [x:xs] ys = min (length (filter ((==)x) xs) + 1) (length (filter ((==)x) ys))

readCode :: String -> Maybe [Int]
readCode str = Nothing

maybe :: (a -> b) b (Maybe a) -> b
maybe f b x = b

allMatches :: [Int] String -> (Int, Int)
allMatches a b = (0,0)

Start = [ matches [4,2,7,1] [1,2,3,4]
  , matches [9,3,0,5] [5,6,7,8]
  , matches [6,6,6,1] [6,6,5,1]
  , matches [5,8,7,9] [9,9,7,8]
  ]