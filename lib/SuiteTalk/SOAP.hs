module SuiteTalk.SOAP
    ( send
    ) where

import           Data.ByteString
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.SOAP                (ResponseParser (DocumentParser), invokeWS)
import           Network.SOAP.Transport.HTTP (initTransportWithM)
import           Text.XML                    (Document, def, parseLBS_)
import           Text.XML.Writer             (ToXML)

send ::
       (ToXML header, ToXML body)
    => String -- SOAP Action
    -> header -- SOAP Action header
    -> body -- SOAP Action body
    -> IO Document
send soapAction header body = do
    transport <- initTransportWithM managerSettings endpointURL pure pure
    invokeWS transport soapAction header body parser
  where
    endpointURL = "https://webservices.netsuite.com/services/NetSuitePort_2018_1"
    managerSettings = tlsManagerSettings
    parser = DocumentParser id
