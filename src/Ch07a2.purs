module Ch07a2 where

import Prelude        (Unit)
import Effect         (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log "Uncomment lines step by step. DERIVE missing functions !!!"
  -- log $ show $ Just 5 == Just 5
  -- log $ show $ Just 5 == Just 2
  -- log $ show $ Just 5 == Nothing
  -- log $ show $ Nothing == Just 5
  -- log $ show $ Nothing == Nothing
  -- log $ show $ Left "left"
  -- log $ show $ Right (Just 42)
  -- log $ show $ Just 1 < Just 5
  -- log $ show $ Just 5 <= Just 5
  -- log $ show $ Just 5 > Just 10
  -- log $ show $ Just 10 >= Just 10
  -- log $ show $ Just 99 > Nothing
  -- log $ show $ Just 99 < Nothing
  -- log $ show $ Just "abc"
  -- log $ show $ Nothing
