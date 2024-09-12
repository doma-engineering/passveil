module Data.HexString ( HexString
                      , hexString
                      , fromBinary
                      , toBinary
                      , fromBytes
                      , toBytes
                      , toText ) where

import           Data.Aeson
import           Data.Word              (Word8)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as BS16 (decode, encode)
import qualified Data.ByteString.Lazy   as BSL

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import qualified Data.Binary            as B (Binary, decode, encode)

-- | Represents a Hex string. Guarantees that all characters it contains

--   are valid hex characters.

data HexString =
  HexString BS.ByteString
  deriving ( Show, Eq, Ord )

instance FromJSON HexString where
  parseJSON = withText "HexString" $ pure . hexString . TE.encodeUtf8

instance ToJSON HexString where
  toJSON = String . toText

-- | Smart constructor which validates that all the text are actually
--   hexadecimal characters.

hexString :: BS.ByteString -> HexString
hexString bs =
  let isValidHex :: Word8 -> Bool
      isValidHex c
        | (48 <= c) && (c < 58)  = True
        | (97 <= c) && (c < 103) = True
        | otherwise              = False

  in if   BS.all isValidHex bs
     then HexString bs
     else error ("Not a valid hex string: " ++ show bs)

-- | Converts a 'B.Binary' to a 'HexString' value

fromBinary :: B.Binary a  => a -> HexString
fromBinary = hexString . BS16.encode . BSL.toStrict . B.encode

-- | Converts a 'HexString' to a 'B.Binary' value

toBinary :: B.Binary a => HexString -> a
toBinary (HexString bs) = B.decode . BSL.fromStrict . bs16decode' $ bs

-- | Reads a 'BS.ByteString' as raw bytes and converts to hex representation. We
--   cannot use the instance Binary of 'BS.ByteString' because it provides
--   a leading length, which is not what we want when dealing with raw bytes.

fromBytes :: BS.ByteString -> HexString
fromBytes = hexString . BS16.encode

-- | Access to the raw bytes in a 'BS.ByteString' format.

toBytes :: HexString -> BS.ByteString
toBytes (HexString bs) = bs16decode' bs

-- | Access to a 'T.Text' representation of the 'HexString'

toText :: HexString -> T.Text
toText (HexString bs) = TE.decodeUtf8 bs

bs16decode' :: BS.ByteString -> BS.ByteString
bs16decode' x = go $ BS16.decode x
  where
    go (Left err) = error err
    go (Right y) = y
