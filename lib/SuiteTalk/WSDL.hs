-- |
-- Module      : SuiteTalk.WSDL
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- Give me a WSDL and I'll give you a service representation.
--
-- You can use the @fetch@ function to download and generate a @WSDL@ model
-- which can be used by the XML and SOAP modules.
--
module SuiteTalk.WSDL where

data WSDL =
    WSDL Endpoint
         [Operation]

type Operation = String

data Endpoint =
    Endpoint Host
             Port

type Path = String

type Identifier = String

type Host = String

type Port = String

mkEndpointURL :: Endpoint -> String
mkEndpointURL (Endpoint host "")   = host
mkEndpointURL (Endpoint host port) = host ++ ":" ++ port
{-
fetch :: IO BsResponse
fetch =
    runReq def $
    req GET
        (https "webservices.netsuite.com" /: "wsdl" /: "v2018_1_0" /: "netsuite.wsdl")
        NoReqBody
        bsResponse
        mempty

parse :: BsResponse -> Either SomeException Document
parse = parseLBS def . BS.fromStrict . responseBody
-}
