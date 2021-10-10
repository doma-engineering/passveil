{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PassVeil.Store.Config
  ( Config(..)
  , read
  , write
  )
where

import Prelude hiding (read)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString

import qualified System.Directory as Directory

import GHC.Generics

import PassVeil.Store.Fingerprint

-- | `Store` configuration. This is not intended to be edited by users but
-- rather an internal format to store information about the `Store`.
data Config = Config
  { whoami :: !Fingerprint -- ^ `Fingerprint` of the `Store` owner
  , signed :: !Bool -- ^ Is the `Store` signed
  }
  deriving
    ( Generic
    , Aeson.FromJSON
    , Aeson.ToJSON
    , Show
    )

-- | Write `Config` to file.
write :: FilePath -> Config -> IO ()
write path = ByteString.writeFile path . Aeson.encode

-- | Read `Config` from file.
read :: FilePath -> IO (Maybe Config)
read path = do
  exists <- Directory.doesFileExist path

  if exists
     then Aeson.decode <$> ByteString.readFile path
     else return Nothing
