module Ch21 where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

newtype StateT s m a = StateT (s -> m (Tuple a s))

-- runStateT
runStateT :: ∀ s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT f) = f

-- Functor
instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT g) = StateT \s -> g s <#> (\(Tuple a b) -> Tuple (f a) b)

-- Apply
instance applyStateT :: Monad m => Apply (StateT s m) where
  apply = ap

-- Applicative
instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

-- Bind
instance bindStateT :: Monad m => Bind (StateT s m) where
  -- bind :: m a -> (a -> m b) -> m b
  -- bind :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  -- bind (StateT x) f = StateT \s -> x s >>= \(Tuple x1 s1) -> case f x1 of StateT x2 -> x2 s1
  bind (StateT x) f = StateT \s -> x s >>= \(Tuple x1 s1) -> runStateT (f x1) s1

-- Monad
instance monadStateT :: Monad m => Monad (StateT s m)

-- MonadState
-- MonadAsk
-- MonadTell
-- MonadThrow
-- MonadError

test :: Effect Unit
test = do
  log "placeholder"
