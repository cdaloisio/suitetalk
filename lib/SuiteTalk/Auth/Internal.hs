-- |
-- Module      : SuiteTalk.Auth.Internal
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- Contains helpers for generating a signature
--
module SuiteTalk.Auth.Internal
    ( generateSignature
    , generateNonce
    , getCurrentTime
    ) where

import           Crypto.Hash
import           Crypto.MAC.HMAC
import qualified Data.ByteString       as B (intercalate)
import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Random         (newStdGen)

import           SuiteTalk.Auth.Types

-- * Signature generation
--
-- | Create a Signature to be used with Netsuite SOAP requests
generateSignature ::
       ConsumerSecret
    -> TokenSecret
    -> Account
    -> ConsumerKey
    -> TokenId
    -> Nonce
    -> Timestamp
    -> Signature
generateSignature consumerSecret tokenSecret account consumerKey tokenId nonce timestamp =
    let signatureData = generateSignatureData account consumerKey tokenId nonce timestamp
        signatureKey = generateSignatureKey consumerSecret tokenSecret
        value = show $ hmacGetDigest (hmac signatureKey signatureData :: HMAC SHA1)
     in Signature HMACSHA256 value

generateNonce :: IO String
generateNonce = do
    g <- newStdGen
    let randomHash = show (hash $ BS.pack $ show g :: Digest SHA1)
    pure $ take 20 randomHash

getCurrentTime :: IO Timestamp
getCurrentTime = do
    currentTime <- getPOSIXTime
    pure $ round $ realToFrac currentTime

generateSignatureKey :: ConsumerSecret -> TokenSecret -> BS.ByteString
generateSignatureKey consumerSecret tokenSecret =
    B.intercalate "&" [BS.pack consumerSecret, BS.pack tokenSecret]

generateSignatureData :: Account -> ConsumerKey -> TokenId -> Nonce -> Timestamp -> BS.ByteString
generateSignatureData account consumerKey tokenId nonce timestamp =
    B.intercalate
        "&"
        [ BS.pack account
        , BS.pack consumerKey
        , BS.pack tokenId
        , BS.pack nonce
        , BS.pack $ show timestamp
        ]
