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

import           Control.Exception
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BS
import           Network.HTTP.Simple
import           Text.XML

data WSDL =
    WSDL Endpoint
         [Operation]

type Operation = String

data Endpoint =
    Endpoint Host
             Port

type Host = String

type Port = String

mkEndpointURL :: Endpoint -> String
mkEndpointURL (Endpoint host "")   = host
mkEndpointURL (Endpoint host port) = host ++ ":" ++ port

data Error
    = ParseError
    | UnknownError

generateWSDLfromURL :: String -> IO (Either Error WSDL)
generateWSDLfromURL url =
    (either (const $ Left ParseError) documentToWSDL . parseResponse) <$> fetchWSDL url

documentToWSDL :: Document -> Either Error WSDL
documentToWSDL document = Right $ WSDL (Endpoint "" "") []

parseResponse :: Response B8.ByteString -> Either SomeException Document
parseResponse = parseLBS def . BS.fromStrict . getResponseBody

fetchWSDL :: String -> IO (Response B8.ByteString)
fetchWSDL wsdlUrl =
    let request = parseRequest_ wsdlUrl
     in httpBS request
