module Simple.Eval.AExp where

import Simple.Data.AExp

eval :: (Show a, Eq a, Num a) => AExp a -> a
eval x = case x of
  Lit n -> n
  Add m n -> (eval m) + (eval n)
  Mul m n -> (eval m) * (eval n)

