{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PassVeil.Store.Cached where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)

import GHC.Generics

import PassVeil.Store.Hash
import PassVeil.Store.Metadata

-- | `Cached` values are stored in the index, and is used to provide
-- unencrypted access to `Metadata`. This is also the place where we keep track
-- of the modification time of a `Hash`'s.
data Cached = Cached
  { hash :: !Hash -- ^ `Hash` of the cached `Content`
  , metadata :: !Metadata -- ^ `Metadata` of the cached `Content`
  , modified :: !UTCTime -- ^ Last time the `Content` File has been modified
  }
  deriving (Generic, FromJSON, ToJSON)
