-- |
-- Module      : SuiteTalk.XML
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- This module contains helpers for constructing valid SOAP requests to
-- Netsuite.
--
module SuiteTalk.XML
    ( buildHeader
    , buildBody
    , Header(..)
    ) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.XML             (Name (..))
import           Text.XML.Writer      (XML, element, elementA)

import           SuiteTalk.Auth       (TokenPassport (..))
import           SuiteTalk.Auth.Types (Signature (..))

newtype Header =
    Header TokenPassport

buildBody :: String -> XML
buildBody soapAction =
    element soapAction' $ do elementA "record" [("recordType", T.pack "state")] ("" :: Text)
  where
    soapAction' = Name (T.pack soapAction) Nothing Nothing

-- | Take a @Header@ (which just wraps the TokenPassport) and convert it to XML
buildHeader :: Header -> XML
buildHeader header =
    case header of
        Header (TokenPassport account consumerKey tokenId nonce timestamp (Signature algorithm value)) ->
            element "tokenPassport" $ do
                element "account" (T.pack account)
                element "consumerKey" (T.pack consumerKey)
                element "token" (T.pack tokenId)
                element "nonce" (T.pack nonce)
                element "timestamp" (T.pack $ show timestamp)
                elementA "signature" [("algorithm", T.pack $ show algorithm)] (T.pack value)
