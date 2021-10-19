{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PassVeil.Store.Content
  ( Content (..),
    create,
    update,
    touch,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import PassVeil.Store.Fingerprint (Fingerprint)
import qualified PassVeil.Store.Generator as Generator
import PassVeil.Store.Metadata (Metadata)
import qualified PassVeil.Store.Metadata as Metadata
import PassVeil.Store.Path (Path)
import PassVeil.Store.Payload (Payload)
import qualified PassVeil.Store.Timestamp as Timestamp

-- | Internal file format for storing secrets in the `Store`. It contains a salt
-- value as well as `Metadata` that is not necessarily secret.
data Content = Content
  { -- | Clear text `Path`
    path :: !Path,
    -- | 64 byte salt value
    salt :: !Text,
    -- | Secret `Payload` we want to protect
    payload :: !Payload,
    -- | Public `Metadata` regarding the `Payload`
    metadata :: !Metadata
  }
  deriving
    ( FromJSON,
      Generic,
      ToJSON,
      Show
    )

-- | Create a new `Content` value.
create ::
  -- | `Path` to locate the `Content`
  Path ->
  -- | `Fingerprint` of the initial owner
  Fingerprint ->
  -- | Payload to be stored in `Content`
  Payload ->
  IO Content
create path' whoami payload' = do
  salt' <- Generator.generate Generator.salt
  timestamp <- Timestamp.new whoami

  return $
    Content
      { path = path',
        salt = salt',
        payload = payload',
        metadata = Metadata.new timestamp
      }

-- | Update `Content` `Payload`. This generates a new salt value and updates the
-- `Metadata` appropriately.
update ::
  -- | `Fingerprint` of the user updating the `Content`
  Fingerprint ->
  -- | New `Payload`
  Payload ->
  -- | Content to update
  Content ->
  IO Content
update whoami payload' content = do
  salt' <- Generator.generate Generator.salt
  timestamp <- Timestamp.new whoami

  let metadata' = Metadata.update timestamp (metadata content)

  return $
    content
      { payload = payload',
        salt = salt',
        metadata = metadata'
      }

-- | Update `Metadata` of `Content`
touch :: Metadata -> Content -> Content
touch metadata' content = content {metadata = metadata'}
