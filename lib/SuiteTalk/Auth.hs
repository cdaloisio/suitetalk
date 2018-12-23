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

import           SuiteTalk.Auth.Internal
import           SuiteTalk.Auth.Types

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
    pure $ TokenPassport account consumerKey tokenId nonce currentTime (signature nonce currentTime)
  where
    signature = generateSignature consumerSecret tokenSecret account consumerKey tokenId
