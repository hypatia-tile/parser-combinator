{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Example02.MpPrac02 where

import Control.Monad (void)
import Control.Applicative (Alternative ())
import Data.Void
import Text.Megaparsec hiding (State, optional)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pScheme' :: Parser String
pScheme' =
  choice
    [ string "data"
    , string "file"
    , string "ftp"
    , string "https"
    , string "http"
    , string "irc"
    , string "mailto"
    ]

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  }
  deriving (Eq, Show)

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttps
  | SchemeHttp
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (String, String) -- (user, password)
  , authHost :: String
  , authPort :: Maybe Int
  }
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme =
  choice
    [ SchemeData <$ string "data"
    , SchemeFile <$ string "file"
    , SchemeFtp <$ string "ftp"
    , SchemeHttps <$ string "https"
    , SchemeHttp <$ string "http"
    , SchemeIrc <$ string "irc"
    , SchemeMailto <$ string "mailto"
    ]

-- for the performance reasone,
--   megaprasec does not backtrack automatically
-- so (a >>= b) <|> (a >>= c) is not the same as a >>= (b <|> c)
alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'

alternativesManually :: Parser (Char, Char)
alternativesManually = (,) <$> char 'a' <*> (char 'b' <|> char 'c')

alternativesWithTracking :: Parser (Char, Char)
alternativesWithTracking =
  try ((,) <$> char 'a' <*> char 'b')
    <|> ((,) <$> char 'a' <*> char 'c')

testAlternatives :: IO ()
testAlternatives = do
  parseTest alternatives "ab"
  parseTest alternatives "ac"
  parseTest alternativesWithTracking "ab"
  parseTest alternativesWithTracking "ac"

optional :: (Alternative f) => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- optional . try $ do
    void (string "//")
    authUser <- optional .try $ do
      user <- some alphaNumChar
      void (char ':')
      password <- some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal)
    return Authority {..}
  return $ Uri {..}

f :: IO ()
f = do
  parseTest (pUri <* eof) "https://mark:secret@example.com"
  parseTest (pUri <* eof) "https://mark:secret@example.com:123"
  parseTest (pUri <* eof) "https://example.com:123"
  parseTest (pUri <* eof) "https://mark@example.com:123"
