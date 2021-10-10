module PassVeil.Store.Uid where

import Data.Text

-- | @gpg@ uid of a `PassVeil.Store.Fingerprint.Fingerprint`.
newtype Uid = Uid { fromUid :: Text }
