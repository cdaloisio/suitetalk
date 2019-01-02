{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Default
import qualified Text.XML                as X

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
                "documentToWSDL"
                [ testCase "check result" $ do
                      document <- X.readFile def "tests/fixtures/netsuite.wsdl.xml"
                      documentToWSDL document @?=
                          Right
                              (WSDL
                                   (Endpoint
                                        "https://webservices.netsuite.com/services/NetSuitePort_2018_1"
                                        "")
                                   ["getAll"])
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
