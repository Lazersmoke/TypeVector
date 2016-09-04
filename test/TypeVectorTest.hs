
import Test.HUnit

main = runTestTT tests

tests = test
  ["5=5" ~: 5 ~=? 5]
