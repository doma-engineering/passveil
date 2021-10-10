{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PassVeil.Store.Trust where

import Data.Aeson (FromJSON, ToJSON)

import GHC.Generics

import PassVeil.Store.Fingerprint (Fingerprint)
import PassVeil.Store.Timestamp (Timestamp)

-- | Encodes trust by pairing up the `Fingerprint` in question, and a
-- `Timestamp` indicating who issued the trust operation.
-- `PassVeil.Store.Log.Log` is used to give additional meaning to this.
data Trust = Trust
  { subject :: !Fingerprint -- ^ Who is this about?
  , timestamp :: !Timestamp -- ^ Who issued it?
  }
  deriving
    ( FromJSON
    , Generic
    , ToJSON
    , Show
    )
