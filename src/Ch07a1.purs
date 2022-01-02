module Ch07a1 where

import Prelude        (Unit, discard, (<>))
import Effect         (Effect)
import Effect.Console (log)

-------------------- JS Primitives --------------------------------------------------------
foreign import ordIntImpl     :: Ordering -> Ordering -> Ordering -> Int -> Int -> Ordering
foreign import eqIntImpl      :: Int -> Int -> Boolean
foreign import showIntImpl    :: Int -> String
foreign import showStringImpl :: String -> String

-------------------- Functions ------------------------------------------------------------
apply :: ∀ a b. (a -> b) -> a -> b
apply f = f

infixr 0 apply as $

lessThan :: ∀ a. Ord a => a -> a -> Boolean
lessThan a b = case compare a b of
  LT -> true
  _  -> false

infixl 4 lessThan as <

lessThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
lessThanOrEq a b = case compare a b of
  GT -> false
  _  -> true

infixl 4 lessThanOrEq as <=

greaterThan :: ∀ a. Ord a => a -> a -> Boolean
greaterThan a b = case compare a b of
  GT -> true
  _  -> false

infixl 4 greaterThan as >

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq a b = case compare a b of
  LT -> false
  _  -> true

infixl 4 greaterThanOrEq as >=

-------------------- Data Types -----------------------------------------------------------
data Maybe a    = Nothing | Just a
data Either a b = Left a | Right b
data Ordering   = LT | GT | EQ

-------------------- Type Classes ---------------------------------------------------------
class Eq a where
  eq :: a -> a -> Boolean

infix 1 eq as ==

-- class Ord a where (would lead to same results in our cases)
class Eq a <= Ord a where
  compare :: a -> a -> Ordering

class Show a where
  show :: a -> String

-------------------- Type Classes Instances -----------------------------------------------
instance eqInt :: Eq Int where
  eq = eqIntImpl

instance showBoolean :: Show Boolean where
  show false = "false"
  show true  = "true"

instance showInt :: Show Int where
  show = showIntImpl

instance showString :: Show String where
  show = showStringImpl

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show (Left a)  = "(Left "  <> show a <> ")"
  show (Right b) = "(Right " <> show b <> ")"

instance showMaybe :: Show a => Show (Maybe a) where
  show Nothing  = "Nothing"
  show (Just a) = "(Just " <> show a <> ")"

instance eqEither :: (Eq a, Eq b) => Eq (Either a b) where
  eq (Left x)  (Left y)  = x == y
  eq (Right x) (Right y) = x == y
  eq _         _         = false

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing  Nothing  = true
  eq (Just x) (Just y) = x == y
  eq _        _        = false

instance ordInt :: Ord Int where
  compare = ordIntImpl LT EQ GT

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing  Nothing  = EQ
  compare Nothing  _        = LT
  compare _        Nothing  = GT
  compare (Just a) (Just b) = compare a b

instance ordEither :: (Ord a, Ord b) => Ord (Either a b) where
  compare (Left  x) (Left  y) = compare x y
  compare (Right x) (Right y) = compare x y
  compare (Left  _) _         = LT
  compare (Right _) (Left  _) = GT

-------------------- Tests ----------------------------------------------------------------
test :: Effect Unit
test = do
  log "Uncomment lines step by step. IMPLEMENT BY HAND missing functions !!! No further imports are necessary!"
  log $ show $ Just 5 == Just 5                            -- true
  log $ show $ Just 5 == Just 2                            -- false
  log $ show $ Just 5 == Nothing                           -- false
  log $ show $ Nothing == Just 5                           -- false
  log $ show $ Nothing == (Nothing :: Maybe Int)           -- true                     -- added type annotation
  log $ show $ (Left "left" :: Either String Int)          -- (Left "left")            -- added type annotation
  log $ show $ (Right (Just 42) :: Either Int (Maybe Int)) -- (Right (Just 42))        -- added type annotation
  log $ show $ Just 1 < Just 5                             -- true
  log $ show $ Just 5 <= Just 5                            -- true
  log $ show $ Just 5 > Just 10                            -- false
  log $ show $ Just 10 >= Just 10                          -- true
  log $ show $ Just 99 > Nothing                           -- true
  log $ show $ Just 99 < Nothing                           -- false
  log $ show $ Just "abc"                                  -- (Just "abc")
  log $ show $ (Nothing :: Maybe Int)                      -- Nothing                  -- added type annotation
