{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PassVeil.Store.Hash
  ( Hash
  , compute
  , fromHash
  , toFilePath
  , fromFilePath
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Hashable (Hashable)
import qualified Data.HexString as HexString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Crypto.Hash.SHA256 as SHA256

import PassVeil.Store.Path (Path)
import qualified PassVeil.Store.Path as Path

-- | @passveil@ stores `Content` using a filename generated from the @SHA256@ of
-- its `Path`. This is done to obfuscate the keys since we do not store them in
-- plain text.
newtype Hash = Hash { fromHash :: Text }
  deriving newtype (Eq, Hashable, FromJSON, ToJSON)

-- | Compute a `Hash` from a `Path`.
compute
  :: Path
  -> Hash
compute path =
  let path' = Text.encodeUtf8 (Path.fromPath path)
      hex = HexString.fromBytes (SHA256.hash path')
   in Hash (HexString.toText hex)

-- | Convert `Hash` to `FilePath`
toFilePath :: Hash -> FilePath
toFilePath = Text.unpack . fromHash

-- | Convert `FilePath` to `Hash`
fromFilePath :: FilePath -> Hash
fromFilePath = Hash . Text.pack
