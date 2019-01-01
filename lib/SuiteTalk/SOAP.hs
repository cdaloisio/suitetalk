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

import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.SOAP                (ResponseParser (DocumentParser), invokeWS)
import           Network.SOAP.Transport.HTTP (initTransportWithM, printBody, printRequest)
import           Text.XML                    (Document)
import           Text.XML.Writer             (ToXML)

import           SuiteTalk.WSDL              (WSDL (..), mkEndpointURL)

-- | Make a request to NetSuite via SuiteTalk
send ::
       (ToXML header, ToXML body)
    => WSDL -- ^ WSDL containing endpoint and operations
    -> String -- ^ SOAPAction to run
    -> header -- ^ SOAPAction header
    -> body -- ^ SOAPAction body
    -> IO Document
send (WSDL endpoint operations) soapAction header body = do
    transport <- initTransportWithM managerSettings endpointURL printRequest printBody
    invokeWS transport soapAction header body documentParser
  where
    endpointURL = mkEndpointURL endpoint
    managerSettings = tlsManagerSettings
    documentParser = DocumentParser id
