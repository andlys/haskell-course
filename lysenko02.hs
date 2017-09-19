{-# OPTIONS_GHC -Wall #-}
module Lysenko02 where

type Code = String

data Move = Move Code Int Int
          deriving (Show, Eq)

-- 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches (x:xs) (y:ys) | (x == y)  = 1 + (exactMatches xs ys)
                           | otherwise = 0 + (exactMatches xs ys)
{-
test:
exactMatches "1589" "5891"
exactMatches "1589" "1159"
-}
-- 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = (map (\n -> 
                          (countif (\chr ->
                                       (chr2int chr) == n)
                                   cd))
                      [0..9])

countif :: (Char -> Bool ) -> Code -> Int
countif p xs = length (filter p xs)

chr2int :: Char -> Int
chr2int chr = fromEnum chr - fromEnum '0'
{-
test:
countDigits "1589"
countDigits "1117"
-}
-- 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = foldl (+) 0
                     (merge (countDigits cd)
                            (countDigits att))

merge :: [Int] -> [Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = (min x y) : (merge xs ys)
{-
test:
matches "1589" "1158"
-}
-- 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = let
                   f = (exactMatches cd att)
                   p = (matches cd att)
                 in (Move att f (p - f))
{- test
getMove "1589" "1158" 
-}
-- 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move att f p) cd = (Move att f p) == (getMove cd att)
{- test
isConsistent (Move "2257" 1 1) "2519"  = True
isConsistent (Move "2257" 1 1) "2529"  = False
-}
-- 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cds = filter (\cd -> isConsistent mv cd)
                            cds

-- 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes n = comb n []

-- adds all possible digits to a list of strings
comb :: Int -> [String] -> [String]
comb 0 lst = lst
comb n []  = comb (n - 1)
                  (addDigits [])
comb n lst = comb (n - 1)
                  (concat
                      (map (\str -> addDigits str)
                           lst))

-- adds all possible digits to a string and returns a list of these
addDigits :: String -> [String]
addDigits str = map (\chr -> str ++ [chr])
                    ['0','1','2','3','4','5','6','7','8','9']

-- 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = solveHelper cd (allCodes (length cd))

solveHelper :: Code -> [Code] -> [Move]
solveHelper cd cds = let mv = getMove cd (head cds)
                     in  mv : 
                         (if (isFound mv) then []
                          else (solveHelper cd
                                           (filterCodes mv
                                                       (tail cds))))

-- checks if mv is the desired one
isFound :: Move -> Bool
isFound (Move att f p) = (f == (length att)) &&
                         (p == 0)
