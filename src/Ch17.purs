module Ch17 where

import Prelude
import Effect         (Effect)
import Effect.Console (log)

{-
 Writing Applicative Instance for Maybe

 Create the Maybe Type from scratch providing all of the prerequisites for Applicative:
 • Define the Type
 • Create a Show Instance using genericShow (for our test code)
 • Create a Functor Instance
 • Create an Apply Instance
 • Create an Applicative Instance
-}

{-
 Writing Applicative Instance for Either

 Instead of writing Eq and Functor, we’re going to derive it and let the compiler do some work for us.
 • Define the type
 • Derive Eq Instance
 • Derive Ord Instance (we’re going to need this later on)
 • Derive Functor Instance
 • Create a Show Instance using genericShow (for our test code)
 • Create a Bifunctor Instance (we’re going to need this later on)
 • Create an Apply Instance
 • Create an Applicative Instance
-}

{-
 Validation

 • Derive a Newtype Instance
 • Derive Functor Instance
 • Derive Bifunctor Instance
 • Create Apply Instance
 • Create Applicative Instance
 • Derive Show Instance
-}

-- newtype Validation err result = Validation (Either err result)

test :: Effect Unit
test = do
  log $ "Ch. 17 Applicatives. Just follow the white rabbit."
  -- log "Writing Applicative Instance for Maybe"
  -- log $ show $ (+) <$> Just 21 <*> Just 21
  -- log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  -- log $ show $ pure (+) <*> Just 17 <*> Just 25
  -- log "Writing Applicative Instance for Either"
  -- -- Associative Composition Law: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  -- log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- -- Identity Law:                pure identity <*> x = x
  -- log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- -- Homomorphism Law:            pure (f x) = pure f <*> pure x
  -- log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- -- Interchange Law:             u <*> pure x = pure (_ $ x) <*> u
  -- log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
  -- log "Using Validation"
  -- log $ show age  -- 10
  -- log $ show age' -- (Age' 10)
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 10  }
  -- log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0   }
  -- log $ show $ createFamilyAges { fatherAge: Age 4,   motherAge: Age 3,   childAge: Age 10  }
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 100 }
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 3,   childAge: Age 0   }
