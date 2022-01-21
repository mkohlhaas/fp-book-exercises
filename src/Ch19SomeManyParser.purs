module Ch19SomeManyParser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array as A
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as U
import Effect (Effect)
import Effect.Console (log)

-- The Parsing State is going to need to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what’s left of the String to the next Parser who takes a stab at parsing what’s left. Also, if a
-- single Parser in the chain were to fail, we want to short-circuit the parsing and return the error, hopefully
-- with some useful information as to what went wrong.
------------------------------
-- Data Types and Type Classes
------------------------------
class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

data PError
  = EOF
  | InvalidChar String

type ParserState a
  = Tuple String a

type ParseFunction e a
  = ParserError e => String -> Either e (ParserState a)

newtype Parser e a
  = Parser (ParseFunction e a)

data Threeple a b c
  = Threeple a b c

instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s

instance applyParser :: Apply (Parser e) where
  apply = ap

instance applicativeParser :: Applicative (Parser e) where
  pure a = Parser \s -> Right $ Tuple s a

instance bindParser :: Bind (Parser e) where
  bind p f =
    Parser \s -> do
      Tuple s1 x <- parse p s
      parse (f x) s1

instance monadParser :: Monad (Parser e)

instance altParser :: Alt (Parser e) where
  alt p1 p2 =
    Parser \s -> case parse p1 s of
      Left _ -> parse p2 s
      Right x -> Right x

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar s = InvalidChar s

char :: ∀ e. Parser e Char
char =
  Parser \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

count :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
count n p
  | n < 0 = pure []
  | otherwise = sequence (A.replicate n p)

count' :: ∀ e a f. Traversable f => U.Unfoldable f => Int -> Parser e a -> Parser e (f a)
count' n p
  | n < 0 = pure U.none
  | otherwise = sequence (U.replicate n p)

count'' :: ∀ e. Int -> Parser e Char -> Parser e String
count'' n p = fromCharArray <$> count n p

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = char >>= \c -> if pred c then pure c else fail $ invalidChar expected

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" $ isDecDigit <<< codePointFromChar

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" $ isAlpha <<< codePointFromChar

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

----------------------
-- Helper Functions --
----------------------

atMost :: ∀ e f a. U.Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n <= 0 = pure U.none
  | otherwise = optional U.none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost A.cons n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

range :: ∀ e f a. Semigroup (f a) => Traversable f => U.Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min > max, min < 0, max <= 0 = pure U.none
  | otherwise = count' min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range A.cons min max p

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (show c) (_ == c)

-- digitsToNum :: String -> Int
-- digitsToNum = fromMaybe 0 <<< fromString
---------------------------
-- Some and Many Parsers --
---------------------------
-------------------------
-- some = one and many --
-- many = some or none --
-------------------------
-- some rhymes with 1  --
-------------------------
-- some :: ∀ e a. Parser e a -> Parser e (Array a)
-- some p = A.cons <$> p <*> many p
-- some :: ∀ e a. Parser e a -> Parser e (Array a)
-- some p = A.cons <$> p <*> defer \_ -> many p
-- many :: ∀ e a. Parser e a -> Parser e (Array a)
-- many p = some p <|> pure []
instance lazyParser :: Lazy (Parser e a) where
  defer f = Parser \s -> parse (f unit) s

-- some :: ∀ e a f. U.Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)
-- some cons p = cons <$> p <*> defer \_ -> many cons p
some :: ∀ e a f. U.Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

many :: ∀ e a f. U.Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure U.none

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty A.(:) <$> some A.(:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many A.(:) p

-------------------------
-- Using some and many --
-------------------------
digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

-- (\d{1,4}), ([a-zA-Z ]+)([0-9]*)
ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  digits1 <- range' 1 4 digit
  constChar ','
  constChar ' '
  letters <- some' (letter <|> constChar' ' ')
  digits2 <- many' digit
  pure [ digits1, letters, digits2 ]

test :: Effect Unit
test = do
  log "Ch. 19 Some and Many Parser."
  -- log $ show $ parse' (some digit) "2343423423abc"
  -- log $ show $ parse' (many digit) "_2343423423abc"
  -- log $ show $ parse' (some digit) "_2343423423abc"
  log $ show $ parse' (some' digit) "2343423423abc"
  log $ show $ parse' (many' digit) "_2343423423abc"
  log $ show $ parse' (some' digit) "_2343423423abc"
  log $ show $ parse' ugly "17, some words"
  log $ show $ parse' ugly "5432, some more words1234567890"
