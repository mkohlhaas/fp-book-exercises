module Ch19 where

import Prelude
import Effect         (Effect)
import Effect.Console (log)

-----------
-- Maybe --
-----------

-- Define data type
-- Create Show instance
-- Create Functor instance
-- Create Apply instance
-- Create Applicative instance
-- Create Bind instance
-- Create Monad instance

------------
-- Either --
------------

-- Define data type
-- Create Show instance
-- Derive Functor instance
-- Create Apply instance
-- Create Applicative instance
-- Create Bind instance
-- Create Monad instance

test :: Effect Unit
test = do
  log "Ch. 19 Coding Monads."
  log "-- Maybe Monad --"
  -- log $ show $ Just (_ * 10) <*> Just 20
  -- log $ show $ Just (_ * 10) <*> pure 20
  -- log $ show $ Just 20 >>= pure <<< (_ * 10)
  -- log $ show do
  --    x <- Just 20
  --    let y = x * 10
  --    pure y
  -- log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
  -- log $ show do
  --    _ <- Just 20
  --    y <- Nothing
  --    pure $ y + 42
  log "-- Either Monad --"
  -- log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
  -- log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
  -- log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  -- log $ show do
  --   x <- Right 20 :: Either Unit _
  --   let y = x * 10
  --   pure y
  -- log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
  -- log $ show do
  --   _ <- Right 20
  --   y <- Left "error"
  --   pure $ y + 42
