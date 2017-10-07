{-# OPTIONS_GHC -Wall #-}
module Lysenko05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
   (<=) Zero Zero = True
   (<=) Zero (Pred _) = False
   (<=) (Pred _) Zero = True
   (<=) (Succ _) Zero = False
   (<=) Zero (Succ _) = True
   (<=) (Pred x) (Pred y) = x <= y
   (<=) (Succ x) (Succ y) = x <= y
   (<=) (Pred _) (Succ _) = True
   (<=) (Succ _) (Pred _) = False

{-
--test
(Zero <= Zero) == True
(Zero <= (Pred Zero)) == False
(Zero <= (Succ Zero)) == True
((Succ Zero) <= (Succ (Succ Zero))) == True
(Zero <= (Pred Zero)) == False
((Succ Zero) <= (Succ Zero)) == True
((Pred (Pred Zero)) <= (Pred Zero)) == True
((Pred (Pred Zero)) <= (Pred (Pred Zero))) == True
((Pred Zero) <= (Pred (Pred Zero))) == False
-}

-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Succ x) = succ $ aiToInteger x
aiToInteger (Pred x) = pred $ aiToInteger x

{-
--test
(aiToInteger Zero) == 0
(aiToInteger (Pred Zero)) == -1
(aiToInteger (Succ Zero)) == 1
(aiToInteger (Succ (Succ Zero))) == 2
(aiToInteger (Pred (Pred Zero))) == -2
(aiToInteger (Succ (Pred Zero))) == 0 -- impossible by terms
-}

-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs Zero x@(Pred _) = x
plusAbs Zero x@(Succ _) = x
plusAbs x@(Pred _) Zero = x
plusAbs x@(Succ _) Zero = x
plusAbs (Pred x) (Pred y) = Pred $ Pred $ plusAbs x y
plusAbs (Succ x) (Succ y) = Succ $ Succ $ plusAbs x y
plusAbs (Succ x) (Pred y) = plusAbs x y
plusAbs (Pred x) (Succ y) = plusAbs x y

{-
--test
plusAbs (Pred (Pred Zero))  (Succ (Succ Zero)) == Zero
plusAbs (Pred Zero)  (Succ (Succ Zero))   ==  Succ Zero
plusAbs (Succ (Succ Zero)) (Pred (Pred Zero)) == Zero
plusAbs (Succ Zero) (Succ Zero) == Succ (Succ Zero)
plusAbs (Pred Zero) (Pred Zero) == Pred (Pred Zero)
plusAbs (Pred Zero) (Pred (Pred Zero)) == (Pred (Pred (Pred Zero)))
-}

-- getter
getAbs :: AbstractInteger -> AbstractInteger
getAbs Zero = Zero
getAbs (Pred x) = x
getAbs (Succ x) = x
-- getter
positiveAbs :: AbstractInteger -> Bool
positiveAbs (Succ _) = True
positiveAbs _ = False
-- getter
negativeAbs :: AbstractInteger -> Bool
negativeAbs = (not.positiveAbs)

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero
timesAbs (Succ Zero) x = x
timesAbs x (Succ Zero) = x
timesAbs (Pred Zero) (Pred Zero) = Succ Zero
timesAbs y x@(Pred Zero) | positiveAbs y = Pred $ timesAbs (getAbs y) x
                         | otherwise = Succ $ timesAbs (getAbs y) x
timesAbs x@(Pred Zero) y = timesAbs y x
timesAbs x@(Pred _) (Pred y) = plusAbs (timesAbs x (Pred Zero)) $ timesAbs x y
timesAbs x y = plusAbs x $ timesAbs x $ getAbs y

-- handy testing for timesAbs
timesAbsTest :: Integer -> Integer -> Integer -> Bool
timesAbsTest a b result = (fromInteger result) == timesAbs (fromInteger a)
                                                           (fromInteger b)

{-
--test
timesAbs (Succ (Succ Zero)) (Pred Zero) == fromInteger (-2)
timesAbs (Pred (Pred Zero)) (Pred (Pred (Pred Zero))) == (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
timesAbsTest 1 0 0
timesAbsTest 0 1 0
timesAbsTest (-1) 0 0
timesAbsTest 0 (-1) 0

timesAbsTest 1 1 1
timesAbsTest (-1) 1 (-1)
timesAbsTest (-1) (-1) 1
timesAbsTest 3 (-1) (-3)
timesAbsTest (-1) 3 (-3)

timesAbsTest 2 2 4
timesAbsTest 3 2 6
timesAbsTest (-1) 4 (-4)
timesAbsTest 4 (-1) (-4)
timesAbsTest 1 (-3) (-3)
timesAbsTest (-3) 1 (-3)
timesAbsTest (-2) (-3) 6
-}

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate  Zero = Zero
    negate (Pred x) = Succ $ negate x
    negate (Succ x) = Pred $ negate x
    fromInteger n | n == 0 = Zero
                  | n < 0  = Pred $ fromInteger $ n + 1
                  | otherwise = Succ $ fromInteger $ n - 1
    abs     Zero = Zero
    abs    (Pred x) = Succ $ abs x
    abs    x@(Succ _) = x
    signum  Zero = 0
    signum (Pred _) = -1
    signum (Succ _) = 1

-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial n | n == 1    = 1
            | otherwise = n * factorial (n - 1)

{-
--test
factorial (Succ Zero)
factorial (Succ (Succ Zero))
factorial (Succ (Succ (Succ Zero))) == 6
factorial (Succ 3) == 24
factorial (Succ 4) == 120
factorial (Succ 5) == 720
factorial (Succ 6) == 5040
-}

-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    show = undefined

-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion = undefined

-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion = undefined

--- Задача 10 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate      = undefined
    fromInteger = undefined
    abs         = undefined
    signum      = undefined
