{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Default
import qualified Text.XML                as X
import qualified Text.XML.Cursor         as C

import           SuiteTalk.Auth.Internal
import           SuiteTalk.Auth.Types
import           SuiteTalk.WSDL

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" unitTests

unitTests =
    [ testGroup
          "SuiteTalk.WSDL"
          [ testGroup
                "serviceUrlMatches"
                [ testCase "check result" $ do
                      document <- X.readFile def "tests/fixtures/netsuite.wsdl.xml"
                      serviceUrlMatches' (C.fromDocument document) @?=
                          [["https://webservices.netsuite.com/services/NetSuitePort_2018_1"]]
                ]
          , testGroup
                "operationMatches"
                [ testCase "check result" $ do
                      document <- X.readFile def "tests/fixtures/netsuite.wsdl.xml"
                      operationMatches' (C.fromDocument document) @?=
                          [ ["login"]
                          , ["ssoLogin"]
                          , ["mapSso"]
                          , ["changePassword"]
                          , ["changeEmail"]
                          , ["logout"]
                          , ["add"]
                          , ["delete"]
                          , ["search"]
                          , ["searchMore"]
                          , ["searchMoreWithId"]
                          , ["searchNext"]
                          , ["update"]
                          , ["upsert"]
                          , ["addList"]
                          , ["deleteList"]
                          , ["updateList"]
                          , ["upsertList"]
                          , ["get"]
                          , ["getList"]
                          , ["getAll"]
                          , ["getSavedSearch"]
                          , ["getCustomizationId"]
                          , ["initialize"]
                          , ["initializeList"]
                          , ["getSelectValue"]
                          , ["getItemAvailability"]
                          , ["getBudgetExchangeRate"]
                          , ["getCurrencyRate"]
                          , ["getDataCenterUrls"]
                          , ["getPostingTransactionSummary"]
                          , ["getServerTime"]
                          , ["attach"]
                          , ["detach"]
                          , ["updateInviteeStatus"]
                          , ["updateInviteeStatusList"]
                          , ["asyncAddList"]
                          , ["asyncUpdateList"]
                          , ["asyncUpsertList"]
                          , ["asyncDeleteList"]
                          , ["asyncGetList"]
                          , ["asyncInitializeList"]
                          , ["asyncSearch"]
                          , ["getAsyncResult"]
                          , ["checkAsyncStatus"]
                          , ["getDeleted"]
                          ]
                ]
          ]
    , testGroup
          "SuiteTalk.Auth.Internal"
          [ testGroup
                "generateNonce"
                [ testCase "check length" $ do
                      nonce <- generateNonce
                      length nonce @?= 20
                , testCase "check duplicates" $ do
                      nonce <- generateNonce
                      nonce' <- generateNonce
                      (nonce /= nonce') @? nonce <> " - should be different to - " <> nonce'
                ]
          , testGroup
                "generateSignature"
                [ testCase "check result" $ do
                      let signature = generateSignature "123" "456" "abc" "789" "876" "543" 12345678
                      signature @?= Signature HMACSHA256 "d5c1637ad2698ccecb4e8f65659029bf23e4eef2"
                ]
          , testGroup
                "getCurrentTime"
                [ testCase "getCurrentTime" $ do
                      currentTime <- getCurrentTime
                      length (show currentTime) @?= 10
                ]
          ]
    ]
