{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module PassVeil.Store.Generator
  ( Generator
  , parse
  , generate
  , salt
  )
where
import Control.Applicative ((<|>), many, optional, some)
import Control.Monad.Except (throwError)

import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Char as Char
import qualified Data.Text as Text

import qualified System.Random as Random

import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec

type Alphabet = [Char]
type Source = [Alphabet]
type Parser = Parsec Void Text

-- | Generators are use to generate random data according to a set of
-- requirements and alphabets. They can be used to generate salts and/or
-- passwords.
data Generator  = Generator
  { generatorLength :: !Int
  , generatorAlphabet :: !Alphabet
  , generatorRequired :: ![(Int, Alphabet)]
  } deriving Show

toSource :: Generator -> Source
toSource Generator{..} =
  let required = concatMap (uncurry replicate) generatorRequired
      len = generatorLength - length required
   in replicate len generatorAlphabet ++ required

shuffle :: Source -> IO Source
shuffle source = go source []
  where
    go [] acc = return acc
    go [a] acc = return (a:acc)
    go src acc = do
      idx <- Random.randomRIO (0, length src - 1)
      let (front, (s:back)) = splitAt idx src

      go (front ++ back) (s:acc)

digit :: Alphabet
digit = ['0' .. '9']

upper :: Alphabet
upper = ['A' .. 'Z']

lower :: Alphabet
lower = ['a' .. 'z']

punctuation :: Alphabet
punctuation = "!\"#%&'()*,-./:;?@[\\]_{}"

symbol :: Alphabet
symbol = "$+<=>^`|~"

printable :: Alphabet
printable = digit ++ upper ++ lower ++ punctuation ++ symbol

-- | `Generator` for a 64 character salt made out of printable ASCII characters.
salt :: Generator
salt = Generator 64 printable []

checkIntegrity :: Generator -> Bool
checkIntegrity Generator{..} =
  sum (map fst generatorRequired) < generatorLength

-- | Create a new `Generator` from an input `Text` which describes the length of
-- the data that should be produced, an optional default alphabet (enclosed by
-- ":") and an optional list of requirements (separated by ":"). Not stating the
-- default alphabet will fall back to all printable characters (denoted by
-- @dulps@).
--
-- A password that has to be 16 characters long, containing at least one
-- upper-case letter, one lower case-letter, one digit, one punctuation
-- character and one symbol would be described like so: @16::1u:1l:1d:1p:1s@.
-- This exmaple would use all avaiable character classes to fill up the
-- password.
--
-- This is the EBNF for the `Generator` rules grammar:
--
-- @
-- nonzero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
-- digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
-- number = nonzero, { digit } ;
--
-- class = "d" | "u" | "l" | "p" | "s" ;
-- alphabet = class, { class } ;
--
-- requirement = ":",  number , alphabet ;
--
-- rules = number, [ ":" , ( alphabet  | "" ) , { requirement } ] ;
-- @
--
-- Generators accept the following classes to build up alphabets:
--
-- [@d@]: Digits: @\<0-9>@
-- [@u@]: Uppercase ASCII letters: @\<A-Z>@
-- [@l@]: Lowercase ASCII letters: @\<a-z>@
-- [@p@]: ASCII Punctuation: @!\"\#%&\'()*,-.\/:;?\@[\\]_{}@
-- [@s@]: ASCII Symbols: @\$+\<\=>^\`|~@
parse :: Text -> Either String Generator
parse input = case Parsec.runParser parse' "FORMAT" input of
  Left err -> throwError (Parsec.errorBundlePretty err)
  Right generator | checkIntegrity generator -> return generator
  _ -> throwError "FORMAT: requirements exceed length"

parse' :: Parser Generator
parse' = Generator <$> length' <*> alpha <*> many minimum' <* Parsec.eof
  where
    length' :: Parser Int
    length' = fmap read $
      (:) <$> leadingDigitChar <*> many Parsec.digitChar

    alpha :: Parser Alphabet
    alpha = do
      mAlpha <- optional $
        sep *> fmap concat (many printable')

      return $ case mAlpha of
        Just alpha' | not (null alpha') -> alpha'
        _ -> printable

    minimum' :: Parser (Int, Alphabet)
    minimum' = sep *> requirement

    sep = Parsec.char ':'

    requirement = (,) <$> length' <*> fmap concat (some printable')

    leadingDigitChar = Parsec.satisfy
      (\c -> Char.isDigit c && c /= '0')

    digit' = Parsec.char 'd' *> pure digit
    upper' = Parsec.char 'u' *> pure upper
    lower' = Parsec.char 'l' *> pure lower
    punct' = Parsec.char 'p' *> pure punctuation
    sym' = Parsec.char 's' *> pure symbol

    printable' = digit'
             <|> upper'
             <|> lower'
             <|> punct'
             <|> sym'

-- | Generate `Text` according to `Generator` rules.
generate :: Generator -> IO Text
generate generator = do
  source <- shuffle (toSource generator)
  go source ""
  where
    go [] acc = return (Text.pack acc)
    go (alpha:as) acc = do
      idx <- Random.randomRIO (0, length alpha - 1)

      go as ((alpha !! idx) : acc)
