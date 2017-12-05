{-# OPTIONS_GHC -Wall #-}
module Lysenko12 where

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show, Ord)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr
          deriving (Eq, Show, Ord)

type TypeTable = [(String, Type)]
type TEnv      = TypeTable    -- тобто [(String, Type)]
type Sub       = TypeTable    -- тобто [(String, Type)]

-- Задача 1 -----------------------------------------
-- Передумова: Єлемент, що шукається, є в таблиці
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp elt list = (snd ((filter ((== elt).fst) list)!!0))

-- Задача 2 -----------------------------------------
tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp a b list = let lst = (filter ((a ==).fst) list)
                       in if length lst == 0
                           then b
                           else lookUp a list

-- Задача 3 -----------------------------------------
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp b lst = map fst (filter ((b ==).snd) lst)

-- Задача 4 -----------------------------------------
occurs :: String -> Type -> Bool
occurs name (TFun t1 t2) = (occurs name t1) || (occurs name t2)
occurs name (TVar varname) = name == varname
occurs _ _ = False

-- Задача 5 -----------------------------------------
-- Передумова: Немає функцій визначених користувачем (конструктор Fun)
-- Передумова: Всі змінні типів (з виразів) мають зв"язування в середовищі типів
inferType :: Expr -> TEnv -> Type
inferType (Boolean _) _ = TBool
inferType (Number _) _ = TInt
inferType (Prim str) _ = tryToLookUp str TErr primTypes
inferType (Id str) environment = tryToLookUp str TErr environment
inferType (Cond iff thenn elsee) x
    | thenn2 /= elsee2 || iff2 == TErr || not (isBool iff2) = TErr
    | otherwise = thenn2
      where iff2 = inferType iff x
            thenn2 = inferType thenn x
            elsee2 = inferType elsee x
inferType (App expr1 expr2) environment
    | not (isFun tmp) = TErr
    | inferType expr2 environment == input = output
    | otherwise = TErr
      where tmp = inferType expr1 environment
            (TFun input output) = tmp
inferType (Fun _ expr) environment = inferType expr environment

isBool ::Type -> Bool
isBool TBool = True
isBool _ = False

isFun :: Type -> Bool
isFun (TFun _ _) = True
isFun _ = False

-- Задача 6 -----------------------------------------
applySub :: Sub -> Type -> Type
applySub _ TInt = TInt
applySub _ TBool = TBool
applySub _ TErr = TErr
applySub sub var@(TVar s) = tryToLookUp s var sub
applySub sub (TFun input output) = TFun (applySub sub input) (applySub sub output)

-- Задача 7 -----------------------------------------
unify :: Type -> Type -> Maybe Sub
unify t t' = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs = undefined

-- Задача 8 -----------------------------------------
inferPolyType :: Expr -> Type
inferPolyType e =
    let vx   = ['a' : show n | n <- [(1::Int)..]]
        (_, t, _) = inferPolyType' e [] vx
    in t

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' = undefined

--------------------------------------------------------------------
showT :: Type -> String
showT TInt        = "Int"
showT TBool       = "Bool"
showT (TFun t t') = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a)    = a
showT TErr        = "Type error"

-- Типи базових операцій (примітивів)...
primTypes :: TypeTable
primTypes
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

---------------------------------------------------
-- Допоміжні функції
updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub = map modify tenv
  where modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld = sNew ++ updateTEnv sOld sNew

-- В combineSubs [s1, s2,..., sn], s1 повинна бути *самою останньою*
-- підстановкою і повинна бути застосована *заключною*
combineSubs :: [Sub] -> Sub
combineSubs = foldr1 combine

------------------------------------------------------
-- Вивод мономорфного типу - приклади тестів...
env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Приклади для тестування уніфікації (unify)...
u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Вивод поліморфного типу - приклади тестів...
ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3")))
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+")))
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3"))
              (TFun (TVar "a2") (TVar "a3"))
