module CLASS01 where 
cnt :: Int -> [Int] -> Int
{-
cnt x xs = if null xs
               then 0
           else
               (if x == (head xs) then 1 else 0) + cnt x (tail xs)
-}
cnt x xs = length (filter (== x) xs)
-- cnt 4 [2,4,5,6,7,4]

friq :: [Int] -> [(Int,Int)]
friq xs = if null xs then [] else
          (head xs, 1 + cnt (head xs) (tail xs)):
          friq (filter (/= (head xs)) (tail xs))
-- friq [1,2,1,5,1,2]
add :: Int -> Int -> Int
add x y = x + y
-- add 5 7

sum1 :: [Int] -> Int
sum1 xs = (sum.(map (`div` 2)).(filter even)) xs
-- sum1 [5,2,3,7,4,6]
-- :t length
