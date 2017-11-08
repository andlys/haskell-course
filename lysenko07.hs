{-# OPTIONS_GHC -Wall #-}
module Lysenko07 where

type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

nodeRefId :: BDDNode -> Index
nodeRefId (_, (i, _, _)) = i

nodeLeft :: BDDNode -> NodeId
nodeLeft (_, (_, n, _)) = n

nodeRight :: BDDNode -> NodeId
nodeRight (_, (_, _, n)) = n

-- Çàäà÷à 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool -- Bool
checkSat (0, _) _ = False
checkSat (1, _) _ = True
checkSat (firstNodeId, nodes) env =
  let
    node = head $ filter (\n -> fst n == firstNodeId) nodes
    refId = nodeRefId node
    envValue = snd $ head $ filter (\pair -> fst pair == refId) env
    nextNodeId = if envValue then (nodeRight node) else (nodeLeft node)
  in checkSat (nextNodeId, nodes) env

{-
checkSat (0, []) [(1,True),(2,False)]
checkSat (1, []) [(1,True),(2,False)]
checkSat bdd2 [(1,False),(2,False)]
checkSat bdd2 [(1,False),(2,True)]
checkSat bdd2 [(1,True),(2,False)]
checkSat bdd2 [(1,True),(2,True)]
checkSat bdd7 [(3,True),(2,False),(9,True)]
-}
-- Çàäà÷à 2 -----------------------------------------
sat :: BDD -> [[(Index, Bool)]]
sat (_, []) = []
sat bdd = filter (not.null) $ satHelper bdd []

satHelper :: BDD -> Env -> [[(Index, Bool)]]
satHelper (0, _) _ = [[]]
satHelper (1, _) env = [env]
satHelper (firstNodeId, nodes) env =
  let
    node = head $ filter (\n -> fst n == firstNodeId) nodes
    refId = nodeRefId node
  in (satHelper (nodeLeft node, nodes) (env ++ [(refId, False)])) ++
       (satHelper (nodeRight node, nodes) (env ++ [(refId, True)]))

-- Çàäà÷à 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Prim b)) = Prim $ not b
simplify (Or (Prim x) (Prim y)) = Prim (x || y)
simplify (And (Prim x) (Prim y)) = Prim (x && y)
simplify expr = expr

-- Çàäà÷à 4 -----------------------------------------
restrict :: BExp -> Index -> Bool -> BExp
restrict pat@(Prim _) _ _  = pat
restrict pat@(IdRef i) index b | i == index = Prim b
                               | otherwise  = pat
restrict (Or  x y) i b = simplify $ Or  (restrict x i b) (restrict y i b)
restrict (And x y) i b = simplify $ And (restrict x i b) (restrict y i b)
restrict (Not x) i b   = simplify $ Not (restrict x i b)

-- Çàäà÷à 5 -----------------------------------------
-- Ïåðåäóìîâà: Êîæíà çì³ííà (³íäåêñ) â áóëüîâîìó âèðàç³ (BExp) ç"ÿâëÿºòüñÿ
--    òî÷íî îäèí ðàç â ñïèñêó ³íäåêñ³â (Index); íåìàº ³íøèõ åëåìåíò³â
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) _ _ | b = (1, [])
                       | otherwise = (0, [])
buildBDD' e n xs = (n, buildHelper e n xs)

buildHelper :: BExp -> NodeId -> [Index] -> [BDDNode]
buildHelper _ _ []     = []
buildHelper e n (i:[]) = let
                           leftExp  = restrict e i False
                           rightExp = restrict e i True
                           leftNodeId  = if (Prim True) /= leftExp then 0 else 1
                           rightNodeId = if (Prim True) /= rightExp then 0 else 1
                          in [(n, (i, leftNodeId, rightNodeId))]
buildHelper e n (i:xs) = let
                           leftExp  = restrict e i False
                           rightExp = restrict e i True
                           leftNodeId = n * 2
                           rightNodeId = 1 + n * 2
                          in (n, (i, leftNodeId, rightNodeId)) :
                             (buildHelper leftExp leftNodeId xs) ++
                             (buildHelper rightExp rightNodeId xs)

-- Çàäà÷à 6 ------------------------------------------
-- Ïåðåäóìîâà: Êîæíà çì³ííà (³íäåêñ) â áóëüîâîìó âèðàç³ (BExp) ç"ÿâëÿºòüñÿ
--    òî÷íî îäèí ðàç â ñïèñêó ³íäåêñ³â (Index); íåìàº ³íøèõ åëåìåíò³â
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD = undefined

------------------------------------------------------
-- Ïðèêëàäè äëÿ òåñòóâàííÿ..

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (IdRef 3) (Or (IdRef 2) (And (Not (IdRef 2)) (IdRef 1)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,0)),(9,(3,0,1)),(5,(2,10,11)),(10,(3,0,1)),(11,(3,0,1))])
