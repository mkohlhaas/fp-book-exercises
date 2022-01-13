module Ch17 where

import Prelude
import Effect         (Effect)
import Effect.Console (log)

-----------------------------------------
-- Writing Applicative Instance for Maybe
-----------------------------------------

-- Create the Maybe Type from scratch providing all of the prerequisites for Applicative:
-- Define the Type
-- Create a Show Instance using genericShow (for our test code)
-- Create a Functor Instance
-- Create an Apply Instance
-- Create an Applicative Instance

------------------------------------------
-- Writing Applicative Instance for Either
------------------------------------------

-- Instead of writing Eq and Functor, we’re going to derive it and let the compiler do some work for us.
-- Define the type
-- Derive Eq Instance
-- Derive Ord Instance (we’re going to need this later on)
-- Derive Functor Instance
-- Create a Show Instance using genericShow (for our test code)
-- Create a Bifunctor Instance (we’re going to need this later on)
-- Create an Apply Instance
-- Create an Applicative Instance

-------------
-- Validation
-------------

-- We are simply just wrapping Either in a newtype. This will allow us to leverage Either’s implementation when
-- it suits us, i.e. when Validation’s behavior is identical to Either, we’ll tell the compiler to derive an
-- instance using newtype.
-- With a new Type we also have the option of overriding certain things we don’t like about Either, e.g. how
-- it short-circuits in apply.

-- newtype Validation err result = Validation (Either err result)

-- type FamilyAgesRow  r = ( fatherAge ::  Age,       motherAge  :: Age,       childAge  :: Age      | r )
-- type FamilyNamesRow r = ( fatherName :: FullName , motherName :: FullName , childName :: FullName | r)
--
-- newtype Age          = Age Int
-- newtype FullName     = FullName String
-- newtype Family       = Family     { | FamilyNamesRow (FamilyAgesRow ()) }
-- newtype FamilyAges   = FamilyAges { | FamilyAgesRow () }
-- newtype LowerAge     = LowerAge Int
-- newtype UpperAge     = UpperAge Int
-- data    FamilyMember = Father | Mother | Child

-- Derive a Newtype Instance
-- Derive Functor Instance
-- Derive Bifunctor Instance
-- Derive Eq Instance
-- Derive Ord Instance
-- Create Apply Instance
-- Derive Ord Instance (override Either's default behavior; collect all errors!)
-- Create Applicative Instance
-- Derive Show Instance
-- Make Show instances for Age, FullName, Family, FamilyAges and FamilyMember
-- Write this function
-- validateAge :: LowerAge -> UpperAge -> Age -> FamilyMember -> Validation (Array String) Age
-- Write this function
-- createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges

test :: Effect Unit
test = do
  log "--------------------------------------------------"
  log "Ch. 17 Applicatives. Just follow the white rabbit."
  log "--------------------------------------------------"
  -- log "------------------------------"
  -- log "Applicative Instance for Maybe"
  -- log "------------------------------"
  -- log $ show $ (+) <$> Just 21 <*> Just 21                     -- (Just 42)
  -- log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)       -- (Just 42)
  -- log $ show $ pure (+) <*> Just 17 <*> Just 25                -- (Just 42)
  -- log "-------------------------------"
  -- log "Applicative Instance for Either"
  -- log "-------------------------------"
  -- -- Associative Composition Law: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  -- log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- -- Identity Law:                pure identity <*> x = x
  -- log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- -- Homomorphism Law:            pure (f x) = pure f <*> pure x
  -- log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- -- Interchange Law:             u <*> pure x = pure (_ $ x) <*> u
  -- log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
  -- log "----------"
  -- log "Validation"
  -- log "----------"
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 10  }  -- (Validation (Right (FamilyAges { childAge: (Age 10), fatherAge: (Age 40), motherAge: (Age 30) })))
  -- log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0   }  -- (Validation (Left ["Father is too old", "Mother is too old", "Child is too young"]))
  -- log $ show $ createFamilyAges { fatherAge: Age 4,   motherAge: Age 3,   childAge: Age 10  }  -- (Validation (Left ["Father is too young", "Mother is too young"]))
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 100 }  -- (Validation (Left ["Child is too old"]))
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 3,   childAge: Age 0   }  -- (Validation (Left ["Mother is too young", "Child is too young"]))
