{-# OPTIONS_GHC -Wall #-}
module Lysenko02 where

-- ��� - ������ ������ ������� - ���������� ���� '0' ..'9'
type Code = String

-- ���� ��� (Move) ���� ����������� Move �������������� ������ (Code) � ��� �����:  
--    ������� "����" � "����"  � ����������-����� �� ��������� �� ����-����� 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- ������ 1 -----------------------------------------
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
-- ������ 2 -----------------------------------------
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
-- ������ 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = foldl max 0
                     (merge (countDigits cd)
                            (countDigits att))

merge :: [Int] -> [Int] -> [Int]
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = [x + y] ++ (merge xs ys)
{-
test:
matches "1589" "1158"
-}
-- ������ 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove = undefined

-- ������ 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- ������ 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- ������ 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes = undefined
   
-- ������ 7 -----------------------------------------
solve :: Code -> [Move]
solve = undefined
 
