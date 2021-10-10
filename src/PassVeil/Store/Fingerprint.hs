{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PassVeil.Store.Fingerprint
  ( Fingerprint(..)
  , toFilePath
  , toString
  )
where

import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text

-- | @gpg@ fingerprint
newtype Fingerprint = Fingerprint
  { fromFingerprint :: Text }
  deriving newtype
    ( Eq
    , FromJSON
    , FromJSONKey
    , Hashable
    , Ord
    , Show
    , ToJSON
    , ToJSONKey
    )

-- | Convert `Fingerprint` into `FilePath`
toFilePath :: Fingerprint -> FilePath
toFilePath = toString

-- | Convert `Fingerprint` into `String`
toString :: Fingerprint -> String
toString = Text.unpack . fromFingerprint
