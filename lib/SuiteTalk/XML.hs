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
    ( build
    , Header(..)
    ) where

import           Text.XML        (Document)
import           Text.XML.Writer (ToXML, soap)

import           SuiteTalk.Auth  (TokenPassport)

newtype Header =
    Header TokenPassport

type Headers = [Header]

build :: (ToXML headers, ToXML body) => headers -> body -> Document
build = soap
