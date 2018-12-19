import           Test.Tasty
import           Test.Tasty.HUnit

import           SuiteTalk.Auth

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [testCase "example" $ 1 @?= 1]
