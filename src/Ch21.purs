module Ch21 where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

newtype StateT s m a = StateT (s -> m (Tuple a s))

-- runStateT
runStateT :: ∀ s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT f) = f

-- functor
instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT g) = StateT \s -> map (\(Tuple a b) -> Tuple (f a) b) (g s)

-- apply
-- instance applyStateT :: Apply (StateT s m) where
--   apply (StateT f) (StateT g) = StateT \s -> map ((\(Tuple h s1) -> (\(Tuple x s2) -> Tuple (h x) s2)) (f s)) (g s)

-- applicative

-- bind

-- monad

test :: Effect Unit
test = do
  log "placeholder"
