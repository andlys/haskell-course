{-# OPTIONS_GHC -Wall #-}
module Lysenko08 where

import Data.List (sort)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Ord, Show)

-- ������ 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) | x == y = isPrefix xs ys
                       | otherwise = False

-- ������ 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition str1 str2 = partHelper str1 str2 []

partHelper :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
partHelper [] str2 accum = (accum, [], str2)
partHelper str1 [] accum = (accum, str1, [])
partHelper (x:xs) (y:ys) accum | x == y = partHelper xs ys (accum ++ [x])
                               | otherwise = (accum, x:xs, y:ys)

-- ������ 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes str@(_:xs) = str : suffixes xs

-- ������ 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring str1 str2 = any (isPrefix str1) (suffixes str2)

-- ������ 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings [] _ = []
findSubstrings prefix str = map fst $ filter ((isPrefix prefix).snd) $ zip [0..] $ (suffixes) str

-- ������ 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node tuples) = sort $ foldl (++) [] $ map (getIndices.snd) tuples

isLeaf ::SuffixTree -> Bool
isLeaf (Node _)= False
isLeaf (Leaf _)= True

getLeafVal :: SuffixTree -> Int
getLeafVal (Node _) = error "unsupported"
getLeafVal (Leaf n) = n

getNodeVal :: SuffixTree -> [(String, SuffixTree)]
getNodeVal (Node t) = t
getNodeVal (Leaf _) = error "unsupported"

-- ������ 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf n) = [n]
findSubstrings' str (Node tuples) = head $ filter (not.null) $ map
    (\tuple -> let a = fst tuple
                   node = snd tuple
               in (if isPrefix str a then
                    getIndices node
                    else (if isPrefix a str then
                          findSubstrings' (drop (length a) str) node
                          else [])))
    tuples

-- ������ 8 -----------------------------------------
haveCommonPrefix::String -> String -> Bool
haveCommonPrefix str1 str2 = let (a, _ , _) = partition str1 str2
                              in (not.null) a

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (_, _) tree@(Leaf _)     = tree
insert (s, n) (Node [])         = Node [(s, Leaf n)]
insert (s, n) (Node (t@(a, tree):tuples))
  | (null.fst3) (partition a s) = Node (t:(getNodeVal (insert (s, n) (Node tuples))))
  | isPrefix a s                = let s_p = drop (length a) s
                                  in (Node ((a, insert (s_p, n) tree):tuples))
  | otherwise                   = let (first, second, third) = partition a s
                                  in (Node ((first, (Node [(second, tree), (third, Leaf n)])):tuples))

fst3 :: (a,a,a) -> a
fst3 (a, _, _) = a

-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

{-
findSubstrings’ "an" (buildTree s1) = [1, 3]
findSubstrings’ "s" (buildTree s2)  = [2, 3, 5, 6]
-}

-- ������ 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = undefined


------------------------------------------------------
-- �������� ������ � ��������� �����..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]
