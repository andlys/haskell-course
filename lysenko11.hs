{-# OPTIONS_GHC -Wall #-}
module Lysenko11 where

import Data.Maybe

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення
--     (закінчує своє обчислення оператором return e)
--   Оператор return завжди останній оператор для виконання в блоку процедури
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value |
           Var Id |
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp]
         deriving (Eq, Show)

data VarDef  =  Arr Id  | Int Id
               deriving (Eq, Show)
type FunDef  =  (Id, ([VarDef], Exp))

data Scope = Local | Global
           deriving (Eq, Show)
type Binding = (Id, (Scope, Value))
type State = [Binding]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue :: Id -> State -> Value
-- Передумова: Значення змінної Id є в стані State
getValue x state = lookUp x (map (\(k, v) -> (k, snd v)) state)

-- Задача 2 -----------------------------------------
getLocals :: State -> State
getLocals [] = []
getLocals (binding@(_, (Local, _)):rest) = binding : getLocals rest
getLocals ((_, (Global, _)):rest) = getLocals rest

getGlobals :: State -> State
getGlobals [] = []
getGlobals ((_, (Local, _)):rest) = getGlobals rest
getGlobals (binding@(_, (Global, _)):rest) = binding : getGlobals rest

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
-- Аргументи - масив, індекс і (нове) значення відповідно
-- Передумова: Три аргумента (Value)  мають значення відповідного типу
--     (масив (A),  ціле (I) і ціле (I)) відповідно.
assignArray (A contents) (I index) (I value) =
    A ((index, value) : (filter ((index /=).fst) contents))
assignArray val _ _ = val

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> State -> State
updateVar (i, newval) [] = [(i, (Local, newval))]
updateVar pair@(i, newval) (binding@(bid, (scope, _)):rest)
    | i == bid = (bid, (scope, newval)) : rest -- stopping recursion
    | otherwise = binding : updateVar pair rest-- continuing recursion

-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Index (A []) _ = I 0
applyOp Index (A ((i, v):rest)) v2@(I index) | i == index = I v
                                             | otherwise = applyOp Index (A rest) v2
applyOp Index _ _ = error "unsupported operation"
applyOp Add (I x) (I y) = I (x + y)
applyOp Add _ _ = error "unsupported operation"
applyOp Minus (I x) (I y) = I (x - y)
applyOp Minus _ _ = error "unsupported operation"
applyOp Mul (I x) (I y) = I (x * y)
applyOp Mul _ _ = error "unsupported operation"
applyOp Less (I x) (I y) = I (if x < y then 1 else 0)
applyOp Less _ _ = error "unsupported operation"
applyOp Equal (I x) (I y) = I (if x == y then 1 else 0)
applyOp Equal _ _ = error "unsupported operation"

-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> State
-- Передумова: списки мають однакову довжину
bindArgs [] _ = []
bindArgs _ [] = []
bindArgs (i:is) (v:vs) = (i, (Local, v)) : bindArgs is vs

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> State -> Value
eval (Const val) _ _ = val
eval (Var i) _ state = getValue i state
eval (OpApp op x y) fs state = applyOp op (eval x fs state) (eval y fs state)
eval (Cond exif exthen exelse) fs state = if (eval exif fs state) == (I 1)
                                            then (eval exthen fs state)
                                            else (eval exelse fs state)
eval (FunApp fid fexs) fs state =
    let fun     = head $ filter ((fid ==).fst) fs
        funvars = head $ map (fst.snd) [fun]
        funexpr = head $ map (snd.snd) [fun]
        values  = evalArgs fexs [fun] state
        bindedState = bindArgs (map getVarId funvars) values
    in (eval funexpr fs bindedState)

getVarId :: VarDef -> Id
getVarId (Arr i) = i
getVarId (Int i) = i

evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs es fs state = map (\e -> eval e fs state) es

-- Задача 8 -----------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> State
executeStatement (Assign i e) fs ps state =
    updateVar (i, (eval e fs state)) state
executeStatement (AssignA i e1 e2) fs ps state =
    updateVar (i, assignArray (getValue i state)
                              (eval e1 fs state)
                              (eval e2 fs state))
              state
executeStatement (If iff thenn elsee) fs ps state =
    if (eval iff fs state) == (I 1)
        then (executeBlock thenn fs ps state)
        else (executeBlock elsee fs ps state)
executeStatement while@(While cond block) fs ps state =
    if (eval cond fs state) == (I 1)
        then executeStatement while fs ps (executeBlock block fs ps state) -- do one iteration
        else state -- quit
executeStatement (Call i pid exprList) fs ps state
  = let (procLocals, procBlock) = lookUp pid ps
        procValues = evalArgs exprList fs state
        callingState = (bindArgs (map getVarId procLocals) procValues) ++ getGlobals   state
        returnState = executeBlock procBlock fs ps callingState
        modifiedState = getLocals state ++ getGlobals returnState
        newBinding = (i, (getValue "$res" returnState))
        newState = if i /= "" then updateVar newBinding modifiedState
                              else modifiedState
    in newState
executeStatement (Return e) fs ps state =
    updateVar ("$res", eval e fs state) state

executeBlock :: Block -> [FunDef] -> [ProcDef] -> State -> State
executeBlock [] _  _ state = state
executeBlock (stmt:rest) fs ps state = executeBlock rest fs ps (executeStatement stmt fs ps state)

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nСпроба знайти  " ++ show x ++
                      " в таблиці котра має лише звязування: " ++
                      show (map fst t)))
              (lookup x t)

-- Стан для тестування
sampleState :: State
sampleState  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7]))]

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs  = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n
  = Const (I n)

-- Реалізація виконання програми
program :: Program -> State
program (dvx, dfx, dpx) =
   let initv :: VarDef -> Binding
       initv (Arr v) = (v, (Global, A []))
       initv (Int v) = (v, (Global, I 0))
       state = map initv dvx
       ( _, bl) = lookUp "main" dpx
   in  executeBlock bl dfx dpx state

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- function  fib(integer n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib
  = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray
  = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1
  = ("sumA1",
     ([Arr "a", Int "n"],
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s")
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd
  = ("gAdd",
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])
