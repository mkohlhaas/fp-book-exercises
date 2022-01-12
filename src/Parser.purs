module Parser where

import Prelude
-- import Data.Either    (Either)
-- import Data.Tuple     (Tuple)
import Effect         (Effect)
import Effect.Console (log)

-- class   ParserError (e :: Type)
-- type    ParserState   a   = Tuple String a
-- type    ParseFunction a e = ParserError e => String -> Either e (ParserState a)
-- newtype Parser        a e = Parser (ParseFunction a e)

test :: Effect Unit
test = do
  log $ "Ch. 17 Parser"
  -- log $ show $ parse  char       "ABC"                          -- (Right (Tuple "BC" 'A')).
  -- log $ show $ parse  twoChars   "ABC"                          -- (Right (Tuple "C" (Tuple 'A' 'B'))).
  -- log $ show $ parse  threeChars "ABC"                          -- (Right (Tuple "" "ABC")).
  -- log $ show $ parse' threeChars "A"                            -- (Left EOF)
  -- log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"  -- (Right (Tuple "" "xyz"))
