module Ch05 where

import Prelude        (Unit)
import Effect         (Effect)
import Effect.Console (log)

-------------------- Tests ----------------------------------------------------------------------------------------------------------------------------
test :: Effect Unit
test = do
  log "Uncomment lines step by step. Implement/import missing functions and all the rest ..."
  -- log (show ((flip const 1 2) == 2))
  -- log $ show $ (flip const 1 2)                                                           == 2
  -- (flip const 1 2) == 2 # show # log
  -- log $ show $ (singleton "xyz")                                                          == ("xyz" : Nil)
  -- log $ show $ (null Nil)                                                                 == true
  -- log $ show $ (null ("abc" : Nil))                                                       == false
  -- log $ show $ (snoc (1 : 2 : Nil) 3)                                                     == (1 : 2 : 3 : Nil)
  -- log $ show $ (length $ 1 : 2 : 3 : Nil)                                                 == 3
  -- log $ show $ (head ("abc" : "123" : Nil))                                               == (Just "abc")
  -- log $ show $ (head Nil)                                                                 == Nothing
  -- log $ show $ (tail Nil)                                                                 == Nothing
  -- log $ show $ (tail ("abc" : "123" : Nil))                                               == (Just ("123" : Nil))
  -- log $ show $ (last Nil)                                                                 == Nothing
  -- log $ show $ (last ("a" : "b" : "c" : Nil))                                             == (Just "c")
  -- log $ show $ (last $ "a" : "b" : "c" : Nil)                                             == (Just "c")
  -- log $ show $ (init Nil)                                                                 == Nothing
  -- log $ show $ (init (1 : Nil))                                                           == (Just Nil)
  -- log $ show $ (init (1 : 2 : Nil))                                                       == (Just (1 : Nil))
  -- log $ show $ (init (1 : 2 : 3 : Nil))                                                   == (Just (1 : 2 : Nil))
  -- log $ show $ (uncons (1 : 2 : 3 : Nil))                                                 == (Just { head: 1, tail: (2 : 3 : Nil) })
  -- log $ show $ (uncons Nil)                                                               == Nothing
  -- log $ show $ (index (1 : Nil) 4)                                                        == Nothing
  -- log $ show $ (index (1 : 2 : 3 : Nil) 1)                                                == (Just 2)
  -- log $ show $ (index Nil 0)                                                              == Nothing
  -- log $ show $ ((1 : 2 : 3 : Nil) !! 1)                                                   == (Just 2)
  -- log $ show $ (findIndex (_ >= 2)  (1 : 2 : 3 : Nil))                                    == (Just 1)
  -- log $ show $ (findIndex (_ >= 99) (1 : 2 : 3 : Nil))                                    == Nothing
  -- log $ show $ (findIndex (10 /= _) Nil)                                                  == Nothing
  -- log $ show $ (findLastIndex (_ == 10) Nil)                                              == Nothing
  -- log $ show $ (findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil))                == (Just 5)
  -- log $ show $ (findLastIndex (_ == 10) (11 : 12 : Nil))                                  == Nothing
  -- log $ show $ (reverse (10 : 20 : 30 : Nil))                                             == (30 : 20 : 10 : Nil)
  -- log $ show $ (concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil))     == (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  -- log $ show $ (filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil))                           == (1 : 2 : 3 : Nil)
  -- log $ show $ (catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)) == (1 : 2 : 5 : Nil)
  -- log $ show $ (range 1 10)                                                               == (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  -- log $ show $ (range 3 ( -3))                                                            == (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  -- log $ show $ (1 .. 10)                                                                  == (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : Nil)
  -- log $ show $ (3 .. ( -3))                                                               == (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
  -- log $ show $ (take 5 (12 : 13 : 14 : Nil))                                              == (12 : 13 : 14 : Nil)
  -- log $ show $ (take 5 ( -7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil))                   == (-7 : 9 : 0 : 12 : -13 : Nil)
  -- log $ show $ (drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil))                                 == (3 : 4 : 5 : 6 : 7 : Nil)
  -- log $ show $ (drop 10 Nil)                                                              == Nil
  -- log $ show $ (takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil))                           == (5 : 4 : Nil)
  -- log $ show $ (takeWhile (_ == -17) (1 : 2 : 3 : Nil))                                   == Nil
  -- log $ show $ (dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil))                           == (3 : 99 : 101 : Nil)
  -- log $ show $ (dropWhile (_ == -17) (1 : 2 : 3 : Nil))                                   == (1 : 2 : 3 : Nil)
  -- log $ show $ (takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil))                                  == (4 : 5 : 6 : Nil)
  -- log $ show $ (takeEnd 10 (1 : Nil))                                                     == (1 : Nil)
  -- log $ show $ (dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil))                                  == (1 : 2 : 3 : Nil)
  -- log $ show $ (dropEnd 10 (1 : Nil))                                                     == Nil
  -- log $ show $ (zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil))                == ((Tuple 1 "a") : (Tuple 2 "b") : (Tuple 3 "c") : Nil)
  -- log $ show $ (zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil))                == ((Tuple "a" 1) : (Tuple "b" 2) : (Tuple "c" 3) : Nil)
  -- log $ show $ (zip Nil (1 : 2 : Nil))                                                    == Nil
  -- log $ show $ (unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil))                    == (Tuple (1 : 2 : 3 : Nil) ("a" : "b" : "c" : Nil))
  -- log $ show $ (unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil))                    == (Tuple ("a" : "b" : "c" : Nil) (1 : 2 : 3 : Nil))
  -- log $ show $ (unzip Nil)                                                                == (Tuple Nil Nil)
