module PassVeil.Store.Identity where

import Data.Text (Text)
import qualified Data.Text as Text

-- | @gpg@ identities are used to look up keys on the keyring.
newtype Identity = Identity { fromIdentity :: Text }

-- | Convert an `Identity` to a `String`.
toString :: Identity -> String
toString = Text.unpack . fromIdentity
