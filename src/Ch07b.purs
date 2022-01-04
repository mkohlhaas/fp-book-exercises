module Ch07b where

import Prelude        (Unit)
import Effect         (Effect)
import Effect.Console (log)

class ToCSV a where
  toCSV :: a -> CSV

-- class FromCSV a where
--   fromCSV :: CSV -> Maybe a

newtype CSV         = CSV String
newtype FullName    = FullName String
newtype Age         = Age Int
data    Occupation  = Doctor | Dentist | Lawyer | Unemployed
data    Person      = Person { name :: FullName , age :: Age , occupation :: Occupation }

test :: Effect Unit
test = do
  log "Uncomment each line. Implement/import missing functions and all the rest ..."
  -- log $ show $ toCSV (Person { name: FullName "Sue Smith" , age: Age 23 , occupation: Doctor }) -- "Sue Smith,23,Doctor"
  -- log $ show $ toCSV (Person { name: FullName "Sue Smith" , age: Age 23 , occupation: Doctor }) == CSV "Sue Smith,23,Doctor"
  -- let person = Person { name: FullName "Sue Smith" , age: Age 23 , occupation: Doctor }
  -- log $ show $ (toCSV person # fromCSV) == Just person
