module Ch19RWS where

import Prelude
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- rewrite Reader, Writer and State data types which are monads for your own reference
-- you might want to comment out the code

newtype Reader r a
  = Reader (r -> a)

newtype Writer w a
  = Writer (Tuple a w)

newtype State s a
  = State (s -> Tuple a s)

-- model a type called RWSResult that contains all three monads
type RWSResult r w s
  = { r :: r, w :: w, s :: s }

-- model a type RWS that works similar to the State monad
newtype RWS r w s a
  = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

-- Keep in mind:
-- Reader = read-only; don't care about the Value on output, only on input
-- Writer = write-only; don't care about the Value on input, only on output
-- State  = read-write

-- write a runRWS function similar to runState
runRWS :: ∀ r w s a. RWS r w s a -> RWSResult r w s -> Tuple a (RWSResult r w s)
runRWS (RWS f) = f

-- write a Functor instance for RWS
instance functorRWS :: Functor (RWS r w s) where
  map f (RWS g) =
    RWS \rws -> case g rws of
      Tuple a res -> Tuple (f a) res

-- write an Apply instance for RWS (you are free to take a shortcut)
instance applyRWS :: Monoid w => Apply (RWS r w s) where
  apply = ap

-- write an Applicative instance for RWS (make sure the Writer is empty on output)
instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  pure a = RWS \{ r, s } -> Tuple a { r, w: mempty, s }

-- write a Bind instance for RWS (make sure the Writer generates the correct log)
instance bindRWS :: Monoid w => Bind (RWS r w s) where
  -- RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
  bind (RWS f) g =
    RWS \rws -> case f rws of
      Tuple a res@{ w } -> case runRWS (g a) res of
        Tuple a' res'@{ w: w' } -> Tuple a' res' { w = w <> w' }

-- write a Monad instance for RWS
instance monadRWS :: Monoid w => Monad (RWS r w s)

---------------------
-- Monad RWS's API --
---------------------

-- write a tell function like Writer's
tell :: ∀ r w s. w -> RWS r w s Unit
tell w = RWS \rws -> Tuple unit rws { w = w }

-- write an ask function like Reader's
ask :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS \{ r, s } -> Tuple r { r, w: mempty, s }

-- write a get function like State's
get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \{ r, s } -> Tuple s { r, w: mempty, s }

-- write a put function like State's
put :: ∀ r w s. s -> Monoid w => RWS r w s Unit
put s = RWS \{ r } -> Tuple unit { r, w: mempty, s: s }

--------------------------------
-- Data Structure for Testing --
--------------------------------

-- for the Reader part
type Config
  = { debugModeOn :: Boolean }

-- for the State part
type Counter
  = Int

rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
  tell [ "test the log" ]
  tell [ "test the log2", "test the log3" ]
  config <- ask
  tell [ "the config is " <> show config ]
  counter <- get
  tell [ "old counter is " <> show counter ]
  put $ counter + 1
  newCounter <- get
  tell [ "new counter is " <> show newCounter ]
  pure unit

test :: Effect Unit
test = log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }

-- (Tuple unit
-- { r: { debugModeOn: true },
--   s: 1,
--   w: ["test the log",
--       "test the log2",
--       "test the log3",
--       "the config is { debugModeOn: true }",
--       "old counter is 0",
--       "new counter is 1"] })
