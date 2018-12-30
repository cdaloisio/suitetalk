-- |
-- Module      : SuiteTalk.XML
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- Give me a WSDL and I'll give you a service representation.
--
module SuiteTalk.WSDL where

data WSDL =
    WSDL Endpoint
         [Namespace]
         [Operation]

type Operation = String

data Endpoint =
    Endpoint Host
             Port

type Namespace = (Path, Identifier)

type Path = String

type Identifier = String

type Host = String

type Port = String
