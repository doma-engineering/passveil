{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module PassVeil.Store.Log (Log(..)) where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

import PassVeil.Store.Trust (Trust)

-- | `Log` tracks all changes to trust. It is used to compute the `Fingerprint`s
-- that are trusted, as well as everyone who had access to the secret `Payload`
-- of the `Content` since the last change.
data Log
  = Allow Trust -- ^ Log added `Trust`
  | Deny Trust -- ^ Log removed `Trust
  deriving Show

allow :: Aeson.Key
allow = "allow"

deny :: Aeson.Key
deny = "deny"

instance FromJSON Log where
  parseJSON (Object o)
    | Just trust <- KeyMap.lookup allow o
    = Allow <$> Aeson.parseJSON trust

    | Just trust <- KeyMap.lookup deny o
    = Deny <$> Aeson.parseJSON trust

  parseJSON invalid =
    Aeson.prependFailure "parsing Log failed, "
      (Aeson.typeMismatch "Object" invalid)

instance ToJSON Log where
  toJSON (Allow trust) = Aeson.object
    [ allow .= trust ]

  toJSON (Deny trust) = Aeson.object
    [ deny .= trust ]
