module Ch19RWS where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-- rewrite Reader, Writer and State data types which are monads for your own reference
-- you might want to comment this code

-- model a type called RWSResult that contains all three monads

-- model a type RWS that works similar to the State monad

-- Note:
-- Reader = read-only; don't care about the Value on output, only on input
-- Writer = write-only; don't care about the Value on input, only on output
-- State  = read-write

-- write a runRWS function similar to runState

-- write a Functor instance for RWS

-- write an Apply instance for RWS (you are free to take a shortcut)

-- write an Applicative instance for RWS (make sure the Writer is empty on output)

-- write a Bind instance for RWS (make sure the Writer generates the correct log)

-- write a Monad instance for RWS

---------------------
-- Monad RWS's API --
---------------------

-- write a tell function like Writer's
-- tell :: ∀ r w s. w -> RWS r w s Unit

-- write an ask function like Reader's
-- ask :: ∀ r w s. RWS r w s r

-- write a get function like State's
-- get :: ∀ r w s. RWS r w s s

-- write a put function like State's
-- put :: ∀ r w s. s -> RWS r w s Unit

---------------------------------
-- Data Structures for Testing --
---------------------------------

-- for the Reader part
type Config
  = { debugModeOn :: Boolean }

-- for the State part
type Counter
  = Int

-- use this test function
-- rwsTest :: RWS Config (Array String) Counter Unit
-- rwsTest = do
--   tell [ "test the log" ]
--   tell [ "test the log2", "test the log3" ]
--   config <- ask
--   tell [ "the config is " <> show config ]
--   counter <- get
--   tell [ "old counter is " <> show counter ]
--   put $ counter + 1
--   newCounter <- get
--   tell [ "new counter is " <> show newCounter ]
--   pure unit

test :: Effect Unit
test =
  log "Ch. 19 RWS Monad"
  -- log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }

-- (Tuple unit
-- { r: { debugModeOn: true },
--   s: 1,
--   w: ["test the log",
--       "test the log2",
--       "test the log3",
--       "the config is { debugModeOn: true }",
--       "old counter is 0",
--       "new counter is 1"] })
