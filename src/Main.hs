module Main where

import           Network.SOAP
import           Network.SOAP.Transport.HTTP

main :: IO ()
main = runCommand

runCommand :: IO ()
runCommand = do
  transport <- initTransport wsdlUrl modify (iconv "cp-1251")
  foundOrders <- listOrders transport True
  print foundOrders

wsdlUrl :: String
wsdlUrl = "https://webservices.netsuite.com/wsdl/v2018_1/netsuite.wsdl"

modify :: RequestP
modify request = undefined

data Order =
  Order Int
  deriving (Show)

listOrders :: Transport -> Bool -> IO [Order]
listOrders t active = undefined
