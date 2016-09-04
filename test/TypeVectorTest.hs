
import Test.HUnit
import Data.TypeVector

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = test
  ["toList works" ~: [1,2 :: Integer] ~=? toList (1 :> 2 :> Nil) 
  ,"fromValue works" ~: (5 :> Nil) ~=? fromValue (5 :: Integer)
  ,"fromPair works" ~: (5 :> 3 :> Nil) ~=? fromPair (5 :: Integer, 3)
  ,"applyVector works" ~: ("Yay!!!" :> "Hmm..." :> Nil) ~=? fromPair ((++"!!!"),(++"...")) `applyVector` fromPair ("Yay","Hmm")
  ,"addVector works" ~: (6 :> 9 :> Nil) ~=? fromPair (3,2) `addVector` fromPair (3,7 :: Integer)
  ,"last works" ~: (4 :: Integer) ~=? Data.TypeVector.last (1 :> 2 :> 3 :> 4 :> Nil)
  ,"append works" ~: ((1 :: Integer) :> 2 :> 3 :> 4 :> Nil) ~=? (1 :> 2 :> Nil) `append` (3 :> 4 :> Nil)
  ]
