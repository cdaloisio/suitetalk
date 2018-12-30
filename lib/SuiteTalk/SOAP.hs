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
import           Network.SOAP.Transport.HTTP (initTransportWithM)
import           Text.XML                    (Document)
import           Text.XML.Writer             (ToXML)

-- | Make a request to NetSuite via SuiteTalk
send ::
       (ToXML header, ToXML body)
    => String -- ^ SOAPAction
    -> header -- ^ SOAPAction header
    -> body -- ^ SOAPAction body
    -> IO Document
send soapAction header body = do
    transport <- initTransportWithM managerSettings endpointURL pure pure
    invokeWS transport soapAction header body documentParser
  where
    endpointURL = "https://webservices.netsuite.com/services/NetSuitePort_2018_1"
    managerSettings = tlsManagerSettings
    documentParser = DocumentParser id
