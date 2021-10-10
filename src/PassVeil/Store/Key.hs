module PassVeil.Store.Key where

import PassVeil.Store.Fingerprint(Fingerprint)
import PassVeil.Store.Hash (Hash)

-- | A `Key` is used to uniquely identify a `Content` value. It consists out of
-- a `Hash` that is the SHA256 of the `Content`s `Path` and the `Fingerprint` of
-- the receiver.
type Key = (Hash, Fingerprint)
