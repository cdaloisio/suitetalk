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
-- You can use the @generateWSDLfromURL@ function to download and generate a
-- @WSDL@ model which can be used by the XML and SOAP modules.
--
module SuiteTalk.WSDL where

import           Control.Exception
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy          as BS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Network.HTTP.Simple
import           Text.XML
import           Text.XML.Cursor

-- * Primary functions
--
-- | Given the URL of a WSDL, download, parse and convert to the WSDL data type
generateWSDLfromURL :: String -> IO (Either Error WSDL)
generateWSDLfromURL url =
    (either (const $ Left XmlParseError) documentToWSDL . parseResponse)
        <$> fetchWSDL url

-- | Create the endpoint URL string from the @Endpoint@ data type
mkEndpointURL :: Endpoint -> String
mkEndpointURL (Endpoint host ""  ) = host
mkEndpointURL (Endpoint host port) = host ++ ":" ++ port

-- * Data types
data WSDL =
    WSDL Endpoint
         [Operation]
    deriving (Eq, Show)

type Operation = String

data Endpoint =
    Endpoint Host
             Port
    deriving (Eq, Show)

type Host = String

type Port = String

data Error
    = XmlParseError
    | NoServiceUrl
    | NoOperations
    deriving (Eq, Show)

-- * Internal helper functions
parseResponse :: Response B8.ByteString -> Either SomeException Document
parseResponse = parseLBS def . BS.fromStrict . getResponseBody

fetchWSDL :: String -> IO (Response B8.ByteString)
fetchWSDL wsdlUrl = httpBS $ parseRequest_ wsdlUrl

documentToWSDL :: Document -> Either Error WSDL
documentToWSDL document =
    let cursor = fromDocument document
    in  WSDL <$> serviceUrlMatches cursor <*> operationMatches cursor

serviceUrlMatches :: Cursor -> Either Error Endpoint
serviceUrlMatches cursor = case serviceUrlMatches' cursor of
    []      -> Left NoServiceUrl
    matches -> Right $ Endpoint (T.unpack $ T.concat $ head matches) ""

operationMatches :: Cursor -> Either Error [Operation]
operationMatches cursor = case operationMatches' cursor of
    []      -> Left NoOperations
    matches -> Right $ map T.unpack $ L.concat matches

serviceUrlMatches' :: Cursor -> [[T.Text]]
serviceUrlMatches' cursor =
    cursor
        $// laxElement "service"
        &/  laxElement "port"
        &/  laxElement "address"
        &|  attribute "location"

operationMatches' :: Cursor -> [[T.Text]]
operationMatches' cursor =
    cursor $// laxElement "portType" &/ laxElement "operation" &| attribute
        "name"
