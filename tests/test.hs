import           Test.Tasty
import           Test.Tasty.HUnit

import           SuiteTalk.Auth.Internal

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [testCase "generateSignature" $ generateSignature "" "" @?= "somesig"]
