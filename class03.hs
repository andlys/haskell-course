
delA :: String -> String
delA [] = []
delA (x:xs) | x == 'a' = delA xs
            | x == 'A' = delA xs
            | otherwise = x : delA xs
-- test:
-- delA "a12eA&"

delA2 :: String -> String
delA2 str = filter (\chr -> chr /= 'a' && chr /= 'A') str
-- test:
-- delA2 "a12eA&"

-- picks the first letter from each string in the input list
fstL :: [String] -> String
fstL = (map head).(filter (not.null))
-- test:
-- fstL ["abc", "12", "def", ""]

{-
-- verbose version of fstL:
fstL :: [String] -> String
fstL lst = map (\str -> head str)
               (filter (\str -> not (null str)) lst)
-}

product1 :: [Int] -> Int
product1 = foldr (*) 1
-- test:
-- product1 [5,2,3]

sum1 :: [Int] -> Int
sum1 = foldl (+) 0
-- test:
-- sum1 [5,2,3]

-- concatenates two strings using foldl
app :: String -> String -> String
app s1 s2 = foldl (++) s1 [s2]
-- app "abc" "123"
