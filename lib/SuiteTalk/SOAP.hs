-- |
-- Module      : SuiteTalk.SOAP
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- Provides a simple wrapper around the Network.SOAP library
--
module SuiteTalk.SOAP
    ( send
    ) where

import  Control.Monad (unless)
import Data.Default (def)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.SOAP                (ResponseParser (DocumentParser), runResponseParser)
import           Network.SOAP.Transport (Transport)
import           Network.SOAP.Transport.HTTP (initTransportWithM, printBody, printRequest)
import           Text.XML                    (Document, Name(..), Element(..), Node(..))
import           Text.XML.Writer             (ToXML, toXML, render, element, document, node)

-- | Make a request to NetSuite via SuiteTalk
send ::
       (ToXML header, ToXML body)
    => String -- ^ SOAPAction
    -> header -- ^ SOAPAction header
    -> body -- ^ SOAPAction body
    -> IO Document
send soapAction header body = do
    transport <- initTransportWithM managerSettings endpointURL printRequest printBody
    invokeWS transport soapAction header body documentParser
  where
    endpointURL = "https://webservices.netsuite.com/services/NetSuitePort_2018_1"
    managerSettings = tlsManagerSettings
    documentParser = DocumentParser id

-- | Prepare data, assemble request and apply a parser to a response.
invokeWS :: (ToXML h, ToXML b)
         => Transport        -- ^ Configured transport to make requests with.
         -> String           -- ^ SOAPAction header.
         -> h                -- ^ SOAP Header element. () or Nothing will result in omiting the Header node. Put a comment if you need an empty element present.
         -> b                -- ^ SOAP Body element.
         -> ResponseParser a -- ^ Parser to use on a request reply.
         -> IO a
invokeWS transport soapAction header body parser =
    transport soapAction doc >>= runResponseParser parser
  where
    !doc = soap header body

-- | Generate a SOAPv1 document.
--
-- Empty header will be ignored.
-- Envelope uses a `soap` prefix.
-- Works great with 'ToXML' class.
--
-- > data BigData = BigData { webScale :: Bool }
-- > instance ToXML BigData where
-- >     toXML (BigData ws) = element ("v" !: "{vendor:uri}bigData") $ toXML ws
-- > let doc = soap () (BigData True)
soap :: (ToXML h, ToXML b)
     => h
     -> b
     -> Document
soap header body = document (sn "Envelope") $ do
    -- Some servers are allergic to dangling Headers...
    unless (null headerContent) $
        node . NodeElement $! Element (sn "Header") def headerContent
    element (sn "Body") (toXML body)

    where sn n = Name n (Just ns) (Just "env")
          ns = "http://schemas.xmlsoap.org/soap/envelope/"
          headerContent = render (toXML header)
