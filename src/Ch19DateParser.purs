module Ch19DateParser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Data.Array (cons)
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

---------------------------------
-- Data Types and Type Classes --
---------------------------------
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

derive instance genericThreeple :: Generic (Threeple a b c) _

instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar s = InvalidChar s

instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s

instance applyParser :: Apply (Parser e) where
  apply f g =
    Parser \s -> do
      Tuple s1 h <- parse f s
      Tuple s2 a <- parse g s1
      pure $ Tuple s2 $ h a

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
      Right x -> Right x
      Left _ -> parse p2 s

----------------------
-- Using the Parser --
----------------------
parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char =
  Parser \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head

count :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n < 0 = pure none
  | otherwise = sequence (replicate n p)

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

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

-----------------
-- Date Parser --
-----------------
newtype Year
  = Year Int

newtype Month
  = Month Int

newtype Day
  = Day Int

data DateFormat
  = YearFirst
  | MonthFirst

type DateParts
  = { year :: Year, month :: Month, day :: Day, format :: DateFormat }

derive newtype instance showYear :: Show Year
derive newtype instance showMonth :: Show Month
derive newtype instance showDay :: Show Day
derive instance genericDateFormat :: Generic DateFormat _
instance showDateFormat :: Show DateFormat where
  show = genericShow

atMost :: ∀ e f a. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

-- atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
-- atMost n p
--   | n <= 0 = pure []
--   | otherwise = optional [] $ p >>= \c -> cons c <$> atMost (n - 1) p
atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost cons n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

range :: ∀ e f a. Semigroup (f a) => Traversable f => Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min > max, min < 0, max <= 0 = pure none
  | otherwise = count min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range cons min max p

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar c = void $ satisfy (show c) (_ == c)

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

-- 1962-10-02
yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- Year <<< digitsToNum <$> count' 4 digit
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  pure {year, month, day, format: YearFirst}

-- 10/2/1962
monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count' 4 digit
  pure {year, month, day, format: MonthFirst}

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst

test :: Effect Unit
test = do
  log "Ch. 19 Date Parser."
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3" -- (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "$_$" -- (Right (Tuple "$_$" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3" -- (Right (Tuple "b2c3" "a1"))
  log $ show $ parse' yearFirst "1962-10-02"
  log $ show $ parse' monthFirst "10/2/1962"
  log $ show $ parse' yearFirst "1999-12-31"
  log $ show $ parse' monthFirst "12/31/1999"
  log $ show $ parse' date "1962-10-02"
  log $ show $ parse' date "10/2/1962"
  log $ show $ parse' date "1999-12-31"
  log $ show $ parse' date "12/31/1999"

