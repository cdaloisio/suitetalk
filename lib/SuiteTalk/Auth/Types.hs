-- |
-- Module      : SuiteTalk.Auth.Types
-- Copyright   : (c) 2018 Chris D'Aloisio
--
-- License     : MPL-2.0
-- Maintainer  : chris.daloisio@bellroy.com
-- Portability : portable
--
module SuiteTalk.Auth.Types
    ( Account
    , ConsumerKey
    , Nonce
    , Timestamp
    , Algorithm(..)
    , Signature(..)
    , TokenId
    , TokenSecret
    , ConsumerSecret
    , Value
    ) where

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
