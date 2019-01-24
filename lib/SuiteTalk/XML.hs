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
module SuiteTalk.XML where

import qualified Data.Text                     as T
import           Text.XML                       ( Name(..) )
import           Text.XML.Writer                ( ToXML
                                                , XML
                                                , element
                                                , elementA
                                                , toXML
                                                )

import           SuiteTalk.Auth                 ( TokenPassport(..) )
import           SuiteTalk.Auth.Types           ( Signature(..) )

-- | Wrap the Header in a newtype for now to allow later expansion for
-- other authentication types
newtype Header =
    Header TokenPassport

-- | Given any type that has an instance for @ToXML@, generate the body for the request
buildBody :: (ToXML a) => a -> String -> XML
buildBody attributes soapAction = element soapAction' $ toXML attributes
    where soapAction' = Name (T.pack soapAction) Nothing Nothing

-- | Take a @Header@ (which just wraps the TokenPassport) and convert it to XML
buildHeader :: Header -> XML
buildHeader header = case header of
    Header (TokenPassport account consumerKey tokenId nonce timestamp (Signature algorithm value))
        -> element "tokenPassport" $ do
            element "account"     (T.pack account)
            element "consumerKey" (T.pack consumerKey)
            element "token"       (T.pack tokenId)
            element "nonce"       (T.pack nonce)
            element "timestamp"   (T.pack $ show timestamp)
            elementA "signature"
                     [("algorithm", T.pack $ show algorithm)]
                     (T.pack value)
