{-# LANGUAGE TupleSections #-}

module PassVeil.Store
  ( Store,
    init,
    load,
    signed,
    toStorePath,
    toFilePath,
    toIndexPath,
    whoami,
    getDirectory,

    -- * Query operations
    member,
    hashes,
    toList,
    doesContentExist,
    filePathForHash,
    verify,

    -- * Modifying operations
    lookup,
    delete,
    insert,
  )
where

import Control.Monad (foldM, forM, unless, when)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import PassVeil.Store.Config (Config (Config))
import qualified PassVeil.Store.Config as Config
import PassVeil.Store.Content (Content)
import PassVeil.Store.Fingerprint (Fingerprint)
import qualified PassVeil.Store.Fingerprint as Fingerprint
import qualified PassVeil.Store.Gpg as Gpg
import PassVeil.Store.Hash (Hash)
import qualified PassVeil.Store.Hash as Hash
import PassVeil.Store.Key (Key)
import PassVeil.Store.Payload (Payload)
import qualified System.Directory as Directory
import System.FilePath ((<.>), (</>))
import Prelude hiding (init, lookup)

-- | A store is locally initialized and is used via the specified `Fingerprint`
-- which idendifies the @gpg@ key used for decryption and signing. An unsigned
-- `Store` does not sign `Content` data via gpg. This option can be useful for
-- `Store`s that are not shared between users and do not need to be verified.
--
-- A `Store` directory includes the following files and directories:
--
-- [@config.json@]: Internal configuration format, containing the currently used
-- `Fingerprint` and configuration information stated at initialization (e.g. if
-- signing is used).
--
-- [@index.json@]: The local `Index` containing decrypted `Metadata`. This is
-- used to quickly search entries in the store. The `Index` is never shared
-- publicly and only contains information that can be decrypted with the
-- `Store`'s @gpg@ key (`whoami`).
--
-- [@store@]: Contains the shared information like encrypted passwords and
-- signatures. Each stored secret has its own directory named using the SHA256
-- of the `Content.path`. This directory contains encrypted `Content`
-- information for each trusted `Fingerprint` as well as an optional @gpg@
-- signature if the `Store` is signed (which is the default).
data Store = Store
  { -- | Directory used for the `Store`
    storePath :: Text,
    -- | @gpg@ key used for decryption and signing
    whoami :: !Fingerprint,
    -- | Is the `Store` signed
    signed :: !Bool
  }

-- | Return filepath for `Store`.
toFilePath :: Store -> FilePath
toFilePath = Text.unpack . storePath

-- | Return filepath to the directory `Store` uses for `Hash` data.
toStorePath :: Store -> FilePath
toStorePath = (</> "store") . toFilePath

-- | Return filepath to the `Store`'s `index` data.
toIndexPath :: Store -> FilePath
toIndexPath store = toFilePath store </> "index.json"

-- | Return filepath to the `Store`'s `Config` data.
toConfigPath :: FilePath -> FilePath
toConfigPath = (</> "config.json")

-- | Initialize a new `Store`.
init ::
  -- | `Fingerprint` of the `Store` owner
  Fingerprint ->
  -- | `Store` filepath
  FilePath ->
  -- | Signed
  Bool ->
  IO Store
init fingerprint path signed' = do
  let config = Config fingerprint signed'

  Directory.createDirectoryIfMissing True path
  Config.write (toConfigPath path) config

  let store = Store (Text.pack path) fingerprint signed'

  return store

-- | Return the default `Store` filepath or compute an alternative path.
getDirectory ::
  -- | Alternative store path
  Maybe FilePath ->
  IO FilePath
getDirectory = maybe defaultStore pure
  where
    defaultStore =
      fmap
        (</> ".passveil")
        Directory.getHomeDirectory

-- | Load `Store` from default file path or alternative path.
load ::
  -- | Alternative store path
  Maybe FilePath ->
  IO (Maybe Store)
load mPath = do
  path <- getDirectory mPath
  mConfig <- Config.read (toConfigPath path)

  forM mConfig $ \config ->
    return $
      Store
        (Text.pack path)
        (Config.whoami config)
        (Config.signed config)

-- | Insert data into `Store`.
--
-- Existing data will be overwritten. This function uses `Gpg.encrypt`
-- internally and can throw an `Gpg.EncryptException`.
insert :: Key -> Content -> Store -> IO ()
insert (hash, fingerprint) content store = do
  createHash
  encrypt store (hash, fingerprint) content
  where
    dir = store' </> Hash.toFilePath hash
    store' = toStorePath store

    createHash = do
      exists <- Directory.doesDirectoryExist dir

      unless exists $
        Directory.createDirectory dir

-- | Check if `Hash` is contained within the `Store`.
member :: Hash -> Store -> IO Bool
member hash store = do
  let dir = toStorePath store </> Hash.toFilePath hash

  Directory.doesDirectoryExist dir

-- | Lookup `Key` in `Store`. This function is using `Gpg.decrypt` internally
-- and can throw a `Gpg.DecryptException`.
lookup :: Key -> Store -> Maybe Payload -> IO (Maybe Content)
lookup key@(hash, fingerprint) store mPayload = do
  exists <- doesContentExist store hash fingerprint

  if exists
    then decrypt store key mPayload
    else return Nothing

-- | Verify if `Key` in `Store` has been issued via the given `Fingerprint`.
verify :: Key -> Fingerprint -> Store -> IO Bool
verify key@(hash, fingerprint) issuer store = do
  exists <- doesContentExist store hash fingerprint

  if exists
     then Gpg.verify
       (signed store)
       (toStorePath store)
       (whoami store)
       key
       issuer

     else return False

-- | Returns visible `Hash` values.
hashes :: Store -> IO [Hash]
hashes store = do
  dirs <- filter boring <$> Directory.listDirectory (toStorePath store)

  let hashes' = map Hash.fromFilePath dirs

  foldM check [] hashes'
  where
    fingerprint = whoami store

    boring = (`notElem` [".", "..", "_darcs"])
    check acc hash = do
      exists <- doesContentExist store hash fingerprint

      return $
        if exists
          then hash : acc
          else acc

-- | Convert visibles values in the `Store` to an associative list. This
-- function is using `Gpg.decrypt` internally and can throw a
-- `Gpg.DecryptException`.
toList :: Store -> Maybe Payload -> IO [(Hash, Content)]
toList store mPayload =
  hashes store >>= traverse fetch >>= return . catMaybes
  where
    fingerprint = whoami store
    fetch hash = fmap (hash,) <$> decrypt store (hash, fingerprint) mPayload

-- | Delete data from `Store`.
delete :: Key -> Store -> IO ()
delete (hash, fingerprint) store = do
  let store' = toStorePath store
      hash' = Hash.toFilePath hash
      fingerprint' = Fingerprint.toFilePath fingerprint

      dir = store' </> hash'
      path' = dir </> fingerprint'

  exists <- doesContentExist store hash fingerprint

  when exists $ do
    Directory.removeFile path'

    when (signed store) $
      Directory.removeFile (path' <.> "sig")

    isEmpty <- null <$> Directory.listDirectory dir

    when isEmpty $
      Directory.removeDirectory dir

-- | Check if `Content` for a given `Hash` and `Fingerprint` exists.
doesContentExist :: Store -> Hash -> Fingerprint -> IO Bool
doesContentExist store hash fingerprint =
  let store' = toStorePath store
      hash' = Hash.toFilePath hash
      fingerprint' = Fingerprint.toFilePath fingerprint

      dir = store' </> hash' </> fingerprint'
   in Directory.doesFileExist dir

decrypt :: Store -> Key -> Maybe Payload -> IO (Maybe Content)
decrypt store = Gpg.decrypt (signed store) (toStorePath store) (whoami store)

encrypt :: Store -> Key -> Content -> IO ()
encrypt store = Gpg.encrypt (signed store) (toStorePath store) (whoami store)

-- | Returns the filepath to the directory of a `Hash`.
filePathForHash :: Store -> Hash -> FilePath
filePathForHash store hash =
  let store' = toStorePath store
      hash' = Hash.toFilePath hash
   in store' </> hash'
