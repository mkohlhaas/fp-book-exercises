module Ch19DateParser where

import Prelude
import Data.Either            (Either(..))
import Data.Maybe             (Maybe(..))
import Data.Tuple             (Tuple(..))
import Data.Array             as A
import Data.Traversable       (sequence)
import Data.Generic.Rep       (class Generic)
import Data.Show.Generic      (genericShow)
import Control.Alt            (class Alt, (<|>))
import Data.String.CodePoints (codePointFromChar)
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.String.CodeUnits  (uncons, fromCharArray)
import Effect                 (Effect)
import Effect.Console         (log)

---------------------------------
-- Data Types and Type Classes --
---------------------------------

class   ParserError (e :: Type) where
  eof         :: e
  invalidChar :: String -> e
data    PError            = EOF | InvalidChar String
type    ParserState     a = Tuple String a
type    ParseFunction e a = ParserError e => String -> Either e (ParserState a)
newtype Parser        e a = Parser (ParseFunction e a)
data    Threeple    a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof           = EOF
  invalidChar s = InvalidChar s

instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s
instance applyParser :: Apply (Parser e) where
  apply f g = Parser \s -> do
     Tuple s1 h <- parse f s
     Tuple s2 a <- parse g s1
     pure $ Tuple s2 $ h a

instance applicativeParser :: Applicative (Parser e) where
  pure a = Parser \s -> Right $ Tuple s a

instance bindParser :: Bind (Parser e) where
  bind p f = Parser \s -> do
    Tuple s1 x <- parse p s
    parse (f x) s1

instance monadParser :: Monad (Parser e)

instance altParser :: Alt (Parser e) where
  alt p1 p2 =  Parser \s -> case parse p1 s of
                              Right x -> Right x
                              Left  _ -> parse p2 s

----------------------
-- Using the Parser --
----------------------

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
                      Nothing            -> Left eof
                      Just {head, tail } -> Right $ Tuple tail head

count :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
count n p | n < 0     = pure []
          | otherwise = sequence (A.replicate n p)

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = char >>= \c -> if pred c then pure c else fail $ invalidChar expected

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" $ isDecDigit <<< codePointFromChar

letter  :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" $ isAlpha <<< codePointFromChar

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

test :: Effect Unit
test = do
  log "Ch. 19 Date Parser."
