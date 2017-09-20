delA :: String -> String
delA [] = []
delA (x:xs) | x == 'a' = delA xs
            | x == 'A' = delA xs
            | otherwise = x : delA xs
-- delA "a12eA&"

delA2 :: String -> String
delA2 str = filter (\chr -> chr /= 'a' && chr /= 'A') str
-- delA2 "a12eA&"

fstL :: [String] -> String
fstL = (map head).(filter (not.null))
-- fstL ["abc", "12", "def"]

product1 :: [Int] -> Int
product1 = foldr (*) 1
-- product1 [5,2,3]

sum1 :: [Int] -> Int
sum1 = foldl (+) 0
-- sum1 [5,2,3]

app :: String -> String -> String
app s1 s2 = foldl (++) s1 [s2]
-- app "abc" "123"