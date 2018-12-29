{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text      (Text)

import           SuiteTalk.Auth (generateTokenPassport)
import           SuiteTalk.XML  (Header (..), build)

main :: IO ()
main = do
    tokenPassport <- generateTokenPassport account consumerKey consumerSecret tokenId tokenSecret
    print $ build (Header tokenPassport) ("" :: Text)

-- TODO: Remove these and add as env variables?
-- Some sample information for testing
account :: String
account = "3186263_SB2"

consumerKey :: String
consumerKey = "19287328c670747608009a72631bde39a0c2b18c38fda03eaf5cb7b473bc8880"

consumerSecret :: String
consumerSecret = "63fb4cfd767632bae841398365087123220c12d57c15c22afc959b2525df5938e"

tokenId :: String
tokenId = "92ca0a8da752b63895945583f165841aab76c9128958f16658ac38dd481ebe"

tokenSecret :: String
tokenSecret = "6f29ce995d68159df881723176286s0ssda767865326cae65d4c16180d07c39a"
