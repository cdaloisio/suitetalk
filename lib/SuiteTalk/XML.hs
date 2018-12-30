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
    , Header(..)
    ) where

import qualified Data.Text            as T
import           Text.XML.Writer      (XML, element, elementA)

import           SuiteTalk.Auth       (TokenPassport (..))
import           SuiteTalk.Auth.Types (Signature (..))

newtype Header =
    Header TokenPassport

-- | Take a @Header@ (which just wraps the TokenPassport) and convert it to XML
buildHeader :: Header -> XML
buildHeader header =
    case header of
        Header (TokenPassport account consumerKey tokenId nonce timestamp (Signature algorithm value)) ->
            element "platformMsgs:tokenPassport" $ do
                element "platformCore:account" (T.pack account)
                element "platformCore:consumerKey" (T.pack consumerKey)
                element "platformCore:token" (T.pack tokenId)
                element "platformCore:nonce" (T.pack nonce)
                element "platformCore:timestamp" (T.pack $ show timestamp)
                elementA
                    "platformCore:signature"
                    [("algorithm", T.pack $ show algorithm)]
                    (T.pack value)
