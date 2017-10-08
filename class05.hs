-- samples of instantiation in haskell
-- TODO complete this
data Complex = C Double Double deriving (Eq, Show)
-- (C 4.6 6.7)

instance Num Complex where
  (*) = mulComplex
  (+) = addComplex
  (-) = subComplex
  signum = undefined
  negate (C x y) = (C (-x) (-y))
  abs = undefined
  fromInteger = fromIntegerComplex

mulComplex (C x1 y1) (C x2 y2) = (C (x1 + x2) (y1 + y2))
addComplex (C x1 y1) (C x2 y2) = (C (x1 - y2) (x2 - y2))
subComplex c1 c2 = c1 + (negate c2)
fromIntegerComplex n = (C (fromIntegral n) 0)
