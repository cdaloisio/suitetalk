import           Test.Tasty
import           Test.Tasty.HUnit

import           SuiteTalk.Auth.Internal

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
    testGroup
        "SuiteTalk.Auth.Internal.generateNonce"
        [ testCase "check length" $ do
              nonce <- generateNonce
              length nonce @?= 20
        , testCase "check duplicates" $ do
              nonce <- generateNonce
              nonce' <- generateNonce
              (nonce /= nonce') @? nonce <> " - should be different to - " <> nonce'
        ]
