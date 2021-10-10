{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PassVeil.Store.Content
  ( Content(..)
  , create
  , update
  , touch
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

import GHC.Generics

import PassVeil.Store.Fingerprint (Fingerprint)
import PassVeil.Store.Metadata (Metadata)
import PassVeil.Store.Path (Path)
import PassVeil.Store.Payload (Payload)
import qualified PassVeil.Store.Generator as Generator
import qualified PassVeil.Store.Metadata as Metadata
import qualified PassVeil.Store.Timestamp as Timestamp

-- | Internal file format for storing secrets in the `Store`. It contains a salt
-- value as well as `Metadata` that is not necessarily secret.
data Content = Content
  { path :: !Path -- ^ Clear text `Path`
  , salt :: !Text -- ^ 64 byte salt value
  , payload :: !Payload -- ^ Secret `Payload` we want to protect
  , metadata :: !Metadata -- ^ Public `Metadata` regarding the `Payload`
  }
  deriving
    ( FromJSON
    , Generic
    , ToJSON
    , Show
    )

-- | Create a new `Content` value.
create
  :: Path -- ^ `Path` to locate the `Content`
  -> Fingerprint -- ^ `Fingerprint` of the initial owner
  -> Payload -- ^ Payload to be stored in `Content`
  -> IO Content
create path' whoami payload' = do
  salt' <- Generator.generate Generator.salt
  timestamp <- Timestamp.new whoami

  return $ Content
    { path = path'
    , salt = salt'
    , payload = payload'
    , metadata = Metadata.new timestamp
    }

-- | Update `Content` `Payload`. This generates a new salt value and updates the
-- `Metadata` appropriately.
update
  :: Fingerprint -- ^ `Fingerprint` of the user updating the `Content`
  -> Payload -- ^ New `Payload`
  -> Content -- ^ Content to update
  -> IO Content
update whoami payload' content = do
  salt' <- Generator.generate Generator.salt
  timestamp <- Timestamp.new whoami

  let metadata' = Metadata.update timestamp (metadata content)

  return $ content
    { payload = payload'
    , salt = salt'
    , metadata = metadata'
    }

-- | Update `Metadata` of `Content`
touch :: Metadata -> Content -> Content
touch metadata' content = content { metadata = metadata' }
