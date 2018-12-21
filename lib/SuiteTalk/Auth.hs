-- |
-- Module      : SuiteTalk.Auth
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- Use this module to construct a valid authentication method for your SOAP service.
--
-- You can use @TokenPassport@ to build your tokenPassport header for your SOAP request.
--
-- It is probably better to use the helper function instead as it will
-- construct the nonce and signature for you.
--
module SuiteTalk.Auth
    ( TokenPassport(..)
    , generateTokenPassport
    ) where

import           Crypto.Hash
import           Crypto.MAC.HMAC
import qualified Data.ByteString       as B (intercalate)
import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import           Data.Time.Clock       (nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Random         (newStdGen)

-- * Token Authentication
--
-- | Data type that contains all values for a valid TokenPassport
data TokenPassport =
    TokenPassport Account
                  ConsumerKey
                  TokenId
                  Nonce
                  Timestamp
                  Signature

type Account = String

type ConsumerKey = String

type Nonce = String

type Timestamp = Int

data Signature =
    Signature Algorithm
              Value

data Algorithm =
    HMACSHA256

type Value = String

type ConsumerSecret = String

type TokenId = String

type TokenSecret = String

-- | Create a valid tokenPassport.
-- You will want to use this to pass in the result to your SOAP client.
generateTokenPassport ::
       Account -- ^ Netsuite Account ID
    -> ConsumerKey -- ^ ConsumerKey from your Netsuite application (under integrations)
    -> ConsumerSecret -- ^ ConsumerSecret from your Netsuite application (under integrations)
    -> TokenId -- ^ Netsuite user access token ID
    -> TokenSecret -- ^ Netsuite user access token secret
    -> IO TokenPassport
generateTokenPassport account consumerKey consumerSecret tokenId tokenSecret = do
    currentTime <- getCurrentTime
    nonce <- generateNonce
    signature <-
        generateSignature consumerSecret tokenSecret account consumerKey tokenId nonce currentTime
    pure $ TokenPassport account consumerKey tokenId nonce currentTime signature

getCurrentTime :: IO Timestamp
getCurrentTime = do
    currentTime <- getPOSIXTime
    pure $ round $ realToFrac currentTime

generateNonce :: IO String
generateNonce = do
    g <- newStdGen
    pure "str"

generateSignature ::
       ConsumerSecret
    -> TokenSecret
    -> Account
    -> ConsumerKey
    -> TokenId
    -> Nonce
    -> Timestamp
    -> IO Signature
generateSignature consumerSecret tokenSecret account consumerKey tokenId nonce timestamp =
    let signatureData =
            B.intercalate
                "&"
                [ BS.pack account
                , BS.pack consumerKey
                , BS.pack tokenId
                , BS.pack nonce
                , BS.pack $ show timestamp
                ] :: BS.ByteString
        signatureKey =
            B.intercalate "&" [BS.pack consumerSecret, BS.pack tokenSecret] :: BS.ByteString
        value = show $ hmacGetDigest (hmac signatureKey signatureData :: HMAC SHA1)
     in pure $ Signature HMACSHA256 value
