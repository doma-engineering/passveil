{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PassVeil.Store.Timestamp where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)
import qualified Data.Time as Time

import PassVeil.Store.Fingerprint (Fingerprint)

import GHC.Generics

-- | Keeps track of when something happens as well as who did it.
data Timestamp = Timestamp
  { at :: !UTCTime
  , by :: !Fingerprint
  }
  deriving
    ( Eq
    , FromJSON
    , Generic
    , ToJSON
    , Show
    )

-- | Create a new `Timestamp` using the current time.
new :: Fingerprint -> IO Timestamp
new fingerprint = do
  now <- Time.getCurrentTime
  return $ Timestamp now fingerprint
