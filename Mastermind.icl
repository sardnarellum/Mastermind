module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -> Int
positionalMatches [] _ = 0
positionalMatches _ [] = 0
positionalMatches [x:xs] [y:ys] = chi (x == y) + positionalMatches xs ys
	where
		chi True  = 1
		chi False = 0

matches :: [Int] [Int] -> Int
matches [] _ = 0
matches _ [] = 0
matches [x:xs] ys = minLength [x:xs] ys + matches (filter ((<>)x) xs) (filter ((<>)x) ys)
	where
		minLength [x:xs] ys = min (length (filter ((==)x) xs) + 1) (length (filter ((==)x) ys))

readCode :: String -> Maybe [Int]
readCode str = resultIfOk (cond str) (map digitToInt (fromString str))
	where
		cond str = size str == 4 && allDigits(fromString str)
		allDigits []     = True
		allDigits [x:xs] = isDigit x && allDigits xs
		resultIfOk False _ = Nothing
		resultIfOk True x  = (Just x)

maybe :: (a -> b) b (Maybe a) -> b
maybe f b (Nothing) = b
maybe f b (Just a) = f a

allMatches :: [Int] String -> (Int, Int)
allMatches a b = (all a b - pos a b, pos a b)
	where
		all a b = maybe (matches a) 0 (readCode b)
		pos a b = maybe (positionalMatches a) 0 (readCode b)

Start = [ positionalMatches_test, matches_test, readCode_test, maybe_test, allMatches_test ]
  
positionalMatches_test =
  [ positionalMatches [4,2,7,1] [1,2,3,4] == 1
  , positionalMatches [9,3,0,5] [5,6,7,8] == 0
  , positionalMatches [6,6,6,1] [6,6,5,1] == 3
  ]
  
matches_test =
  [ matches [4,2,7,1] [1,2,3,4] == 3
  , matches [9,3,0,5] [5,6,7,8] == 1
  , matches [6,6,6,1] [6,6,5,1] == 3
  , matches [5,8,7,9] [9,9,7,8] == 3
  ]
  
readCode_test =
  [ readCode "1234"  == Just [1,2,3,4]
  , readCode "12345" == Nothing
  , readCode "123a"  == Nothing
  ]
  
maybe_test =
  [ maybe ((+) 10) 7 Nothing  == 7
  , maybe ((+) 10) 7 (Just 5) == 15
  ]
  
allMatches_test =
  [ allMatches [4,2,7,1] "1234" == (2, 1)
  , allMatches [9,3,0,5] "1234" == (1, 0)
  , allMatches [9,3,0,5] "123a" == (0, 0)
  ]