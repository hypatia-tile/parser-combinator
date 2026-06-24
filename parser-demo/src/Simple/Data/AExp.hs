module Simple.Data.AExp where

data AExp a
  = Lit a
  | Add (AExp a) (AExp a)
  | Mul (AExp a) (AExp a)
  deriving (Show, Eq)
