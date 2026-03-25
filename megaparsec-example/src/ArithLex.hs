{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ArithLex (mainLex) where

import Control.Applicative (empty, some)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
  ( MonadParsec (eof),
    fancyFailure,
    between,
    many,
    getOffset,
    getSourcePos,
    observing,
    try,
    Parsec,
    ParseErrorBundle ,
    SourcePos (..),
    anySingle,
    errorBundlePretty,
    parse,
    runParser',
    setOffset,
    token,
    unPos,
    withRecovery,
    (<|>), registerFancyFailure
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    space1
  )
import Text.Megaparsec.Error (ErrorFancy (..), ParseError(..), ShowErrorComponent(..))
import Text.Megaparsec.Pos (mkPos)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as Set

--------------------------------------------------------------------------------------------------
-- Span
--------------------------------------------------------------------------------------------------

data Span = Span
  { spanStart :: SourcePos,
    spanEnd :: SourcePos
  }
  deriving (Eq, Ord)

instance Show Span where
  show (Span s e) =
    sourcePosShort s <> " - " <> sourcePosShort e

sourcePosShort :: SourcePos -> String
sourcePosShort p =
  let name = sourceName p
      lineNo = unPos (sourceLine p)
      colNo = unPos (sourceColumn p)
    in name <> ":" <> show lineNo <> ":" <> show colNo

--------------------------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------------------------

data Token
  = TInt Span Int
  | TPlus Span
  | TMul Span
  | TLParen Span
  | TRParen Span
  | TError Span Char
  deriving (Eq, Ord, Show)

tokenSpan :: Token -> Span
tokenSpan = \case
  TInt sp _ -> sp
  TPlus sp -> sp
  TMul sp -> sp
  TLParen sp -> sp
  TRParen sp -> sp
  TError sp _ -> sp

--------------------------------------------------------------------------------------------------
-- Custom lexical error
--------------------------------------------------------------------------------------------------

data LexErr
  = InvalidChar Char
  deriving (Eq, Ord, Show, Generic)

instance ShowErrorComponent LexErr where
  showErrorComponent (InvalidChar c) =
    "invalid character: " <> show c

--------------------------------------------------------------------------------------------------
-- Parser type
--------------------------------------------------------------------------------------------------

type Parser = Parsec LexErr String

--------------------------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------------------------

-- Skip ordinary whitespace. No comments for now.
-- first argument for L.space is white space character
-- second argument is line comment parer, third argument is block comment parser
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--------------------------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------------------------

withSpan :: Parser a -> Parser (Span, a)
withSpan p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  pure (Span start end, x)

symbolTok :: Char -> (Span -> Token) -> Parser Token
symbolTok c mk = do
  (sp, _) <- withSpan (lexeme (char c))
  pure (mk sp)

integerTok :: Parser Token
integerTok = do
  (sp, digits) <- withSpan (lexeme (some digitChar))
  pure (TInt sp (read digits))

--------------------------------------------------------------------------------------------------
-- One good token
--------------------------------------------------------------------------------------------------

goodToken :: Parser Token
goodToken =
      try integerTok
  <|> symbolTok '+' TPlus
  <|> symbolTok '*' TMul
  <|> symbolTok '(' TLParen
  <|> symbolTok ')' TRParen

--------------------------------------------------------------------------------------------------
-- Recovery token
--------------------------------------------------------------------------------------------------

-- If a token cannot be parsed, record a custom error and consume one character.
recoverToken :: ParseError String LexErr -> Parser Token
recoverToken _bundle = do
  start <- getSourcePos
  bad <- anySingle
  end <- getSourcePos
  pure (TError (Span start end) bad)

--------------------------------------------------------------------------------------------------
-- Token parser with reocvery
--------------------------------------------------------------------------------------------------

tokenP :: Parser Token
tokenP =
  withRecovery recoverToken $
    observing goodToken >>= \case
      Right tok -> pure tok
      Left _err -> do
        -- We want a custom lexical error attached to the current position.
        -- Then withRecovery will catch it and call recoverToken.
        start <- getSourcePos
        bad <- anySingle
        end <- getSourcePos
        registerFancyFailure (Set.singleton (ErrorCustom (InvalidChar bad)))
        pure (TError (Span start end) bad) <* sc

--------------------------------------------------------------------------------------------------
-- Whole lexer
--------------------------------------------------------------------------------------------------

tokensP :: Parser [Token]
tokensP = between sc eof (many tokenP)

--------------------------------------------------------------------------------------------------
-- Running and reporting
--------------------------------------------------------------------------------------------------

data LexResult = LexResult
  { lexTokens :: [Token],
    lexErrors :: [String]
  }
  deriving (Show)

-- Run the lexer and keep all TError tokens.
-- Megaparsec itself will recover and continue, so parse succeeds as long as
-- recovery succeeds.
lexArithmetic :: FilePath -> String -> Either String LexResult
lexArithmetic file input =
  case parse tokensP file input of
    Left bundle -> 
      Left (errorBundlePretty bundle)
    Right toks ->
      Right $
        LexResult
          { lexTokens = toks,
            lexErrors = map tokenErrorMessage (filter isErrorToken toks)
          }
isErrorToken :: Token -> Bool
isErrorToken = \case
  TError _ _ -> True
  _          -> False

tokenErrorMessage :: Token -> String
tokenErrorMessage = \case
  TError sp c ->
    "lexical error at " <> show sp <> ": invalid character " <> show c
  _ ->
    error "tokenErrorMessage called on non-error token"


--------------------------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------------------------
prettyToken :: Token -> String
prettyToken = \case
  TInt sp n -> "TInt " <> show n <> " @ " <> show sp
  TPlus sp -> "TPlus @ " <> show sp
  TMul sp -> "TMul @ " <> show sp
  TLParen sp -> "TLParen @ " <> show sp
  TRParen sp -> "TRParen @ " <> show sp
  TError sp c -> "TError " <> show c <> " @ " <> show sp

printLexResult :: LexResult -> IO ()
printLexResult (LexResult toks errs) = do
  putStrLn "Tokens:"
  mapM_ (putStrLn . prettyToken) toks
  putStrLn ""
  putStrLn "Errors:"
  if null errs
    then putStrLn "(none)"
    else mapM_ (putStrLn . (" " <>)) errs

--------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------

mainLex :: IO ()
mainLex = do
  let input1 = "12 + 3 * (45 + 6)"
      input2 = "12 + 3 * (7 # 9&)\n11 + $"

  putStrLn "Input 1 ==="
  putStrLn input1
  case lexArithmetic "example1" input1 of
    Left msg -> putStrLn msg
    Right r  -> printLexResult r

  putStrLn "\n=== Input 2 ==="
  putStrLn input2
  case lexArithmetic "example2" input2 of
    Left msg -> putStrLn msg
    Right r  -> printLexResult r
