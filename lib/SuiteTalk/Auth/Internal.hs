-- |
-- Module      : SuiteTalk.Auth
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
-- Contains helpers to generateSignature
--
module SuiteTalk.Auth.Internal
    ( generateSignature
    ) where

-- Ruby implementation
--
-- def signature
--   Base64.encode64(OpenSSL::HMAC.digest(OpenSSL::Digest.new('sha256'), signature_key, signature_data))
-- end
--
-- def signature_key
--   "#{consumer_secret}&#{token_secret}"
-- end
--
-- def signature_data
--   "#{account}&#{consumer_key}&#{token_id}&#{nonce}&#{timestamp}"
-- end
--
--
-- XML submission
--
--       <platformCore:account>4198333_SB1</platformCore:account>
--      <platformCore:consumerKey>***FILTERED***</platformCore:consumerKey>
--      <platformCore:token>***FILTERED***</platformCore:token>
--      <platformCore:nonce>hpWNM5oyIlzmov4hOTfS</platformCore:nonce>
--      <platformCore:timestamp>1545141020</platformCore:timestamp>
--      <platformCore:signature algorithm="HMAC-SHA256">eCtijG4HACH2JJAib+YVoQIUfVGN94yDhX40GrsA9vU=
generateSignature signatureKey signatureData = "somesig"
