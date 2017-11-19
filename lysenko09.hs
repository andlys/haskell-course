{-# OPTIONS_GHC -Wall #-}
module Lysenko09 where

type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])

type Header = [Attribute]
type Row = [AttValue]
type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue |
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

-- Задача 1 -----------------------------------------
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

-- Задача 2 -----------------------------------------
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove elt tuplets = filter (\(a, _) -> a /= elt)
                            tuplets

-- Задача 3 -----------------------------------------
lookUpAtt :: AttName -> Header -> Row -> AttValue
--Передумова: Імя атрибуту присутнє в заданому заголовку.
lookUpAtt an hdr row = head $ filter (`elem` row) $ (snd.head) $
                          filter (\(name, _) -> name == an) hdr

-- Задача 4 -----------------------------------------
removeAtt :: AttName -> Header -> Row -> Row
removeAtt an hdr row = filter (/= lookUpAtt an hdr row)
                              row

-- Задача 5 -----------------------------------------
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Передумова: Кожний рядок таблиці містить одне значення заданого атрибуту
buildFrequencyTable (_, attvals) (_, rows) =
  freq (filter (`elem` attvals)
               (foldl (++) [] rows))
       attvals

-- function from our classes
cnt :: Eq a => a -> [a] -> Int
cnt _ []      = 0
cnt n (x:xs)  | x == n    = 1 + (cnt n xs)
              | otherwise = (cnt n xs)
-- function from our classes
freq :: Eq a => [a] -> [a] -> [(a,Int)]
freq [] []    = []
freq [] atts  = [(elt, 0) | elt <- atts]
freq (x:xs) _ = (x, 1 + (cnt x xs)) : freq (filter (/= x) xs) []

-- Задача 6 -----------------------------------------
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree  Null _ _         = ""
evalTree (Leaf attval) _ _ = attval
evalTree (Node _ _) [] _  = error "impossible by terms of this task"
evalTree (Node _ _) _ []  = error "impossible by terms of this task"
evalTree tree@(Node attname list) ((currval, _):hs) (a:as)
  | attname == currval = evalTree ((snd.head) $ filter (\(val, _) -> val == a) list) hs as
  | otherwise = evalTree tree hs as

-- Задача 7 -----------------------------------------
partitionData :: DataSet -> Attribute -> Partition
partitionData (hdr, rows) (attname, attvals) =
  [ (val, dataset) | val <- attvals,
                     let newhdr  = remove attname hdr
                         rowlist = map (removeAtt attname hdr) $
                                        filter (elem val) rows
                         dataset = (newhdr, rowlist) ]

-- Задача 8 -----------------------------------------
--
-- Задається...
-- В цьому простому випадку: атрибут, що вибирається - це перший атрибут в заголовку.
--   Зауважимо, що кваліфікуючий атрибут присутній в заголовку,
--   тому його необхідно вилучити з можливих кандидатів.
--
nextAtt :: AttSelector
--Передумова: Заголовок містить по крайній мірі один вхідний атрибут
nextAtt (headerDS, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) headerDS)

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree
buildTree dataset@(hdr, rows) att@(attname, _) selector
  | null rows = Null
  | (allSame.(map (lookUpAtt attname hdr))) rows = Leaf (lookUpAtt attname hdr (head rows))
  | otherwise = let selectedAtt = selector dataset att
                in Node (fst selectedAtt)
                        (map (\(attval, newDataset) ->
                                (attval, buildTree newDataset att selector))
                             (partitionData dataset selectedAtt))

--------------------------------------------------------------------

outlook :: Attribute
outlook
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute
temp
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute
humidity
  = ("humidity", ["high", "normal"])

wind :: Attribute
wind
  = ("wind", ["windy", "calm"])

result :: Attribute
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header
  =  [outlook,    temp,   humidity, wind,    result]
table
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- Це таж сама таблиця, але результат у другій колонці
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header'
  =  [outlook,    result, temp,   humidity, wind]
table'
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook"
         [("sunny", Node "temp"
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity"
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp"
                         [("hot", Null),
                          ("mild", Node "humidity"
                                        [("high",Node "wind"
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity"
                                        [("high", Null),
                                         ("normal", Node "wind"
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook"
         [("sunny", Node "humidity"
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind"
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]

outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
