module Main where

import           Network.SOAP
import           Network.SOAP.Transport.HTTP
import           Network.SOAP.Transport.HTTP.OpenSSL

main :: IO ()
main = runCommand

runCommand :: IO ()
runCommand = do
  managerSettings <- makeSettings Nothing sslSetup
  transport <-
    initTransportWithM managerSettings endpointUrl requestModifier bodyModifier
  foundOrders <- listOrders transport True
  print foundOrders

endpointUrl :: String
endpointUrl = "https://webservices.netsuite.com/wsdl/v2018_1/netsuite.wsdl"

requestModifier :: RequestProc
requestModifier = pure

bodyModifier :: BodyProc
bodyModifier = pure

sslSetup _ = pure ()

data Order =
  Order Int
        String
  deriving (Show)

listOrders :: Transport -> Bool -> IO [Order]
listOrders t active = undefined
