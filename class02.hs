module Lysenko_lab_02 where

-- sth
add10 :: [Int] -> [Int]
add10 xs = map (\x -> x + 10) xs

cnt :: Int -> [Int] -> Int
cnt _ [] = 0
cnt n (x:xs) | x == n = 1 + (cnt n xs)
             | otherwise = (cnt n xs)
--
freq :: [Int] -> [(Int,Int)]
freq [] = []
freq (x:xs) = (x, 1 + (cnt x xs)) : freq (filter (/= x) xs)

--freq [5,1,2,1,5,2,7]