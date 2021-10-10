{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
module PassVeil.Store.Metadata
  ( Metadata(..)
  , HashLog
  , deny
  , allow
  , new
  , trusted
  , insiders
  , update
  )
where

import Prelude hiding (log)

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

import GHC.Generics

import PassVeil.Store.Fingerprint (Fingerprint)
import PassVeil.Store.Log (Log)
import PassVeil.Store.Timestamp (Timestamp)
import PassVeil.Store.Trust (Trust(Trust))
import qualified PassVeil.Store.Log as Log
import qualified PassVeil.Store.Timestamp as Timestamp
import qualified PassVeil.Store.Trust as Trust

-- | `HashMap` of `Fingerprint`s to `Timestamp`s. The `Timestamp`s describe when
-- trust was given to a `Fingerprint` and by whom.
type HashLog = HashMap Fingerprint Timestamp

-- | All meta-data associated with a `Content` value.
data Metadata = Metadata
  { created :: !Timestamp -- ^ When was the `Content` first created
  , updated :: !(Maybe Timestamp) -- ^ Last update
  , issued :: !Timestamp -- ^ Timestamp of file creation
  , log :: ![Log] -- ^ Log of all trust operations
  }
  deriving
    ( FromJSON
    , Generic
    , ToJSON
    , Show
    )

-- | Create new `Metadata` using a `Timestamp`.
new :: Timestamp -> Metadata
new timestamp =
  let whoami = Timestamp.by timestamp
      trust = Trust whoami timestamp
   in Metadata
        { created = timestamp
        , updated = Nothing
        , issued = timestamp
        , log = [Log.Allow trust]
        }

-- | Update `Metadata` using a `Timestamp`. This resets the `Content` `log` to
-- all the `Fingerprint`s that are currently trusted. Untrusted `Fingerprint`s
-- should have no knowledge of the new `Payload` and can be safely removed.
update :: Timestamp -> Metadata -> Metadata
update timestamp metadata = metadata
  { issued = timestamp
  , updated = Just timestamp
  , log = map (Log.Allow . uncurry Trust)
        $ List.sortOn (Timestamp.at . snd)
        $ HashMap.toList
        $ trusted metadata
  }

appendLog
  :: Metadata
  -> Timestamp
  -> (Trust -> Log)
  -> [Fingerprint]
  -> Metadata
appendLog metadata timestamp log' appended = metadata
  { issued = timestamp
  , log = log metadata ++ map (log' . flip Trust timestamp) appended
  }

-- | Allow a list of `Fingerprint`s to have access to a `Content` value.
allow :: Timestamp -> [Fingerprint] -> Metadata -> Maybe Metadata
allow timestamp allowed metadata
  | trusted' <- trusted metadata
  , added <- filter (not . (`HashMap.member` trusted')) allowed
  , not (null added)
  = Just $ appendLog metadata timestamp Log.Allow added

  | otherwise = Nothing

-- | Deny a list of `Fingerprint`s to have access to a `Content` value.
deny
  :: Timestamp
  -> [Fingerprint]
  -> Metadata
  -> Maybe ([Fingerprint], Metadata)
deny timestamp denied metadata
  | trusted' <- trusted metadata
  , removed <- filter (`HashMap.member` trusted') denied
  , not (null removed)
  = Just (removed, appendLog metadata timestamp Log.Deny removed)

  | otherwise = Nothing

foldLog :: (HashLog -> Trust -> HashLog) -> Metadata -> HashLog
foldLog deny' = List.foldl' f HashMap.empty . log
  where
    f acc (Log.Allow trust) = HashMap.insert
      (Trust.subject trust)
      (Trust.timestamp trust)
      acc

    f acc (Log.Deny trust) = deny' acc trust

-- | Return a `HashLog` of all trusted `Fingerprint`s.
trusted :: Metadata -> HashLog
trusted = foldLog delete
  where
    delete acc trust = HashMap.delete
      (Trust.subject trust)
      acc

-- | Return a `HashLog` of all `Fingerprint`s that had access to the `Content`
-- once.
insiders :: Metadata -> HashLog
insiders = foldLog const
