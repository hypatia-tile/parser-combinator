module Example01.Trivial where

-- Let us start by thinking of a parser as a function
--   that takes a string as input and yields some kind of result.
-- In general, however, a parser might not consume the entire input string,
--   so the result of parsing should also include the remaining unconsumed input.
-- Similarly, a parser might fail on its input string.
-- Rather than reporting a run-time error if this happens,
--   we choose to have parsers return a list of pairs rather than a single pair.
type Parser a = String -> [(a, String)]

-- The three primitive parsers

-- always succeed without consuming any of the input string and return
result :: a -> Parser a
result v = \inp -> [(v, inp)]

-- always fails
zero :: Parser a
zero = \_inp -> []

-- consume one character
item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x : xs) -> [(x, xs)]

seqP :: Parser a -> Parser b -> Parser (a, b)
p `seqP` q = \inp ->
  [ ((v, w), inp'')
  | (v, inp') <- p inp,
    (w, inp'') <- q inp'
  ]

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

sat :: (Char -> Bool) -> Parser Char
sat p =
  item `bind` \x ->
    if p x
      then result x
      else zero

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> p inp ++ q inp

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
word = neWord `plus` result ""
  where
    neWord = letter `bind` \x ->
             word `bind` \xs ->
             result (x:xs)

