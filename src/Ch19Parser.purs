module Ch19Parser where

import Prelude
import Data.Either            (Either(..))
import Data.Maybe             (Maybe(..))
import Data.Tuple             (Tuple(..))
import Data.Array             as A
import Data.Unfoldable        as U
import Data.Traversable       (class Traversable, sequence)
import Data.Generic.Rep       (class Generic)
import Data.Show.Generic      (genericShow)
import Data.String.CodeUnits  (uncons, fromCharArray)
import Effect                 (Effect)
import Effect.Console         (log)

-- The Parsing State is going to need to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what’s left of the String to the next Parser who takes a stab at parsing what’s left. Also, if a
-- single Parser in the chain were to fail, we want to short-circuit the parsing and return the error, hopefully
-- with some useful information as to what went wrong.

------------------------------
-- Data Types and Type Classes
------------------------------

-- e = error type, a = return type
class   ParserError (e :: Type) where
  eof :: e
data    PError            = EOF                                                  -- application specific parse error type
type    ParserState     a = Tuple String a                                       -- left-over string and parsed value
type    ParseFunction e a = ParserError e => String -> Either e (ParserState a)
newtype Parser        e a = Parser (ParseFunction e a)
data    Threeple    a b c = Threeple a b c

-- Create a  Functor     instance for Parser (map over the parsed value)
instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s
-- Create an Apply        instance for Parser
instance applyParser :: Apply (Parser e) where
  apply f g = Parser \s -> case parse f s of
                     Left e             -> Left e
                     Right (Tuple s1 h) -> case parse g s1 of
                                               Left e             -> Left e
                                               Right (Tuple s2 a) -> Right $ Tuple s2 $ h a

-- Create an Applicative instance for Parser (this is our Applicative Parser)
instance applicativeParser :: Applicative (Parser e) where
  pure a = Parser \s -> Right $ Tuple s a

-- Create a Bind instance for Parser
instance bindParser :: Bind (Parser e) where
  bind f g = Parser \s -> case parse f s of
                    Left x             -> Left x
                    Right (Tuple s1 a) -> parse (g a) s1

-- Create a Monad instance for Parser
instance monadParser :: Monad (Parser e)

-------------------
-- Using the Parser
-------------------

-- Write a parse function
parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f
-- Use parse in map and apply
-- Create Show instance for PError
derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow
-- Create ParserError instance for PError
instance parserErrorPError :: ParserError PError where
  eof = EOF
-- Write a char parser using String libary function uncons
char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
                      Nothing            -> Left eof
                      Just {head, tail } -> Right $ Tuple tail head

-- Write a two-char parser
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

-- Write a char-two-char parser
threeChars :: ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeChars = Tuple <$> char <*> twoChars

-- Write a Show instance for Threeple
derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow
-- Write a Threeple 3-char parser
threeChars' :: ∀ e. Parser e (Threeple Char Char Char)
threeChars' = Threeple <$> char <*> char <*> char

-- Write a 3-char parser returning a String using library function fromCharArray
threeChars'' :: ∀ e. Parser e String
threeChars'' = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

-- write a parse' function which includes the error type in its signature
parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

-- Write a 10-char parser in the same manner as the 3-char parser
tenChars :: ∀ e. Parser e String
tenChars = (\c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 -> fromCharArray [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10])
           <$> char <*> char <*> char <*> char <*> char <*> char <*> char <*> char <*> char <*> char

-- Do the same using sequence from Data.Traversable and replicate from Data.Array using this helper function
count :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
count n p | n < 0     = pure []
          | otherwise = sequence (A.replicate n p)

-- Make count more generic and call it count'
count' :: ∀ e a f. Traversable f => U.Unfoldable f => Int -> Parser e a -> Parser e (f a)
count' n p | n < 0     = pure U.none
           | otherwise = sequence (U.replicate n p)

test :: Effect Unit
test = do
  log "Ch. 19 Monadic Parser."
  log $ show $ (parse  char         "ABC" :: Either PError _)                           -- (Right (Tuple "BC" 'A')).
  log $ show $ (parse  twoChars     "ABC" :: Either PError _)                           -- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse  threeChars   "ABC" :: Either PError _)                           -- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse  threeChars'  "ABC" :: Either PError _)                           -- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse  threeChars'' "ABC" :: Either PError _)                           -- (Right (Tuple "" "ABC"))
  log $ show $ parse'  char         "ABC"                                               -- (Right (Tuple "BC" 'A')).
  log $ show $ parse'  twoChars     "ABC"                                               -- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse'  threeChars   "ABC"                                               -- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse'  threeChars'  "ABC"                                               -- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse'  threeChars'' "ABC"                                               -- (Right (Tuple "" "ABC"))
  log $ show $ parse'  threeChars   "A"                                                 -- (Left EOF)
  log $ show $ parse'  tenChars     "ABCDEFGHIJKLMNOPQRSTUVXYZ"                         -- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count  10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ"  -- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count' 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ"  -- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
