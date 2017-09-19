{-# OPTIONS_GHC -Wall #-}
module Lysenko01 where
{-
env:
ghc -v
Glasgow Haskell Compiler, Version 7.10.3, stage 2 booted by GHC version 7.8.4
-}

-- 1 -----------------------------------------
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- 2 -----------------------------------------
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- 2 -----------------------------------------
toRevDigits :: Integer -> [Integer]
toRevDigits n = if n <= 0 then []
                else lastDigit n : toRevDigits (dropLastDigit n)
{-
test:
toRevDigits (negate 1)
toRevDigits 0
toRevDigits 1
toRevDigits 12
toRevDigits 123
toRevDigits 1234
-}

-- 3 -----------------------------------------
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = if null lst then []
                       else head lst :
                          (if null (tail lst) then []
                           else ((head (tail lst)) * 2) :
                              doubleEveryOther (tail (tail lst)))

-- 4 -----------------------------------------
sumDigits :: [Integer] -> Integer
sumDigits lst = (sum
                    (map sumSingleNumber
                         lst))
{-
test:
sumDigits [10,5,18,4]  = 19
-}

-- sums digits of a single number
sumSingleNumber :: Integer -> Integer
sumSingleNumber n = sum (toRevDigits n)

-- 5 -----------------------------------------
-- Luhn algorithm
luhn :: Integer -> Bool
luhn n = (0 == (mod (sumDigits
                      (reverse
                        (doubleEveryOther
                          (toRevDigits n))))
                    10))
{-
test:
luhn 5594589764218858 = True
luhn 1234567898765432 = False
-}

-- 6 -----------------------------------------
type Move = (Int,Int)

hanoi :: Integer -> Int -> Int -> Int -> [Move]
hanoi n beg mid fin = if n <= 0 then
                        []
                      else
                        (hanoi (n - 1) beg fin mid) ++
                        [(beg, fin)] ++
                        (hanoi (n - 1) mid beg fin)
{-
test:
hanoi 1 1 2 3
hanoi 2 1 2 3
hanoi 3 1 2 3
-}
