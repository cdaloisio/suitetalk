{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.Exit
import           Text.XML        (Name (..))
import           Text.XML.Writer (ToXML, element, elementA, toXML)

import           SuiteTalk.Auth  (generateTokenPassport)
import           SuiteTalk.SOAP  (send)
import           SuiteTalk.WSDL  (generateWSDLfromURL)
import           SuiteTalk.XML   (Header (Header), buildBody, buildHeader)

--Sample datatypes
--------
data Search =
    Search SearchType
           RecordType
           Value

instance ToXML Search where
    toXML (Search searchType recordType value) =
        elementA
            (Name (T.pack searchType) Nothing Nothing)
            [("recordType", T.pack recordType)]
            (T.pack value)

type SearchType = String

type RecordType = String

type Value = String

-------
main :: IO ()
main = do
    tokenPassport <- generateTokenPassport account consumerKey consumerSecret tokenId tokenSecret
    wsdl <- generateWSDLfromURL "https://webservices.netsuite.com/wsdl/v2018_1_0/netsuite.wsdl"
    case wsdl of
        Left err -> do
            print err
            exitFailure
        Right wsdl' -> do
            let header = buildHeader $ Header tokenPassport
            let body = buildBody $ Search "record" "state" ""
            response <- send wsdl' "getAll" header body
            print response
            putStrLn "Done"

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
