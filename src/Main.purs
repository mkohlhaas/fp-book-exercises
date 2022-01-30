module Main where

import Prelude
import Effect (Effect)
-- import Ch05 as Ch05
-- import Ch07a1 as Ch07a1
-- import Ch07a2 as Ch07a2
-- import Ch07b as Ch07b
-- import Ch09 as Ch09
-- import Ch11 as Ch11
-- import Ch13 as Ch13
-- import Ch15 as Ch15
-- import Ch17 as Ch17
-- import Ch17Parser as P
-- import Ch19 as Ch19
-- import Ch19Parser as P1
-- import Ch19DateParser as DP
import Ch19SomeManyParsers as SM

main :: Effect Unit
main = SM.test
