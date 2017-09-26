{-# OPTIONS_GHC -Wall #-}
module Lysenko03 where

data BinomTree a = Node a Int [BinomTree a]
                   deriving (Eq, Ord, Show)
{-
data BinomTree a = Node {value :: a,
                         order :: Int,
                         children :: [BinomTree a]}
                   deriving (Eq, Ord, Show)
-}
type BinomHeap a = [BinomTree a]

value :: BinomTree a -> a
value (Node v _ _) = v
order :: BinomTree a -> Int
order (Node _ n _) = n
children :: BinomTree a -> BinomHeap a
children (Node _ _ list) = list

-- ������ 1 -----------------------------------------
combineTrees :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
combineTrees (Node a1 k1 list1) (Node a2 k2 list2)
    | (k1 /= k2) = error "incompatible trees"
    | (a1 <= a2) = (Node a1 (k1 + 1) ((Node a2 k2 list2) : list1))
    | otherwise = (Node a2 (k2 + 1) ((Node a1 k1 list1) : list2))

{-
--test
combineTrees t5 t6 == t7
-}

-- ������ 2 -----------------------------------------
extractMin :: Ord a => BinomHeap a -> a
extractMin [] = error "empty heap"
extractMin (x:xs) = foldl (min) (value x) (map value xs)

{-
--test
extractMin []
extractMin h3 == 1
-}

-- ������ 3 -----------------------------------------
mergeHeaps :: Ord a => BinomHeap a -> BinomHeap a -> BinomHeap a
mergeHeaps heap1 [] = heap1
mergeHeaps [] heap2 = heap2
mergeHeaps (x:xs) (y:ys)
    | order x < order y = x : (mergeHeaps xs (y:ys))
    | order x > order y = y : (mergeHeaps (x:xs) ys)
    | otherwise = mergeHeaps (mergeHeaps xs ys)
                            [(combineTrees x y)]

{-
--test
mergeHeaps h4 h5 == h6
-}

-- ������ 4 -----------------------------------------
insert :: Ord a => a -> BinomHeap a -> BinomHeap a
insert val heap = mergeHeaps heap
                            [Node val 0 []]
{-
--test
insert 7 [Node 4 0 []] == [Node 4 1 [Node 7 0 []]]
-}
-- ������ 5 -----------------------------------------
deleteMin :: Ord a => BinomHeap a -> BinomHeap a
deleteMin heap =
    let tree = (filter (\t -> (extractMin heap) == value t) heap)!!0
    in mergeHeaps (reverse (children tree))
                  (filter (/= tree) heap)

{-
--test
deleteMin h6 == h7
-}

-- ������ 6 -----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = []
binomSort list = heapToList (foldr insert [] list)

heapToList :: Ord a => BinomHeap a -> [a]
heapToList [] = []
heapToList heap = (extractMin heap) :
                  (heapToList (deleteMin heap))
{-
--test
binomSort "BinomialHeap" == "BHaaeiilmnop"
-}

-- ������ 7 -----------------------------------------
toBinary :: BinomHeap a -> [Int]
toBinary [] = []
toBinary heap = map (\n ->
                        (length (filter (\tree -> n == order tree)
                                        heap)))
                    (reverse [0..(foldl (max) 0 (map order heap))])
{-
--test
toBinary h2 == [1, 1, 0, 0]
h1
toBinary h1
h2
toBinary h2
h3
toBinary h3
-}

-----------------------------------------------------
-- �������� ������ �����...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinomTree Int
--  ����������: t7 - ��������� ������ t5 � t6

-- t1 .. t4 �'��������� �� ���. 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []],
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 � t6 ����� �� ���.2; t7 - ������ �� ���.2
t5 = Node 4 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- ��������� ������...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- �������� ������ ���...

h1, h2, h3, h4, h5, h6, h7 :: BinomHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 �������� �� ���.3...
h3 = [t1, t2, t4]

-- ��� ��������� ���� ���������������� ����. ���� ����� �� ���.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - ��������� ������ h4 � h5, ������ �� ���.4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 �������� �� ���.5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
