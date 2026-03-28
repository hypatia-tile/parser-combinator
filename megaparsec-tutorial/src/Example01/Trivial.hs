module Example01.Trivial where

-- Let us start by thinking of a parser as a function
--   that takes a string as input and yields some kind of result.
-- In general, however, a parser might not consume the entire input string,
--   so the result of parsing should also include the remaining unconsumed input.
type Parser a = String -> (a, String)
