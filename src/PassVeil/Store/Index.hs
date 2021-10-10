{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module PassVeil.Store.Index
  ( Index
  , empty
  , destroy

  -- * Index operations
  , IndexM

  -- ** Refreshing the `Index`
  , IndexMod(..)
  , refresh

  -- ** Modifying the `Index`
  , clear
  , delete
  , insert

  -- ** Querying the `Index`
  , lookup
  , paths
  , toList
  )
where

import Prelude hiding (lookup)

import Control.Arrow (second)
import Control.Monad (when, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

import qualified System.Directory as Directory

import PassVeil.Store (Store)
import PassVeil.Store.Cached (Cached(Cached))
import PassVeil.Store.Hash (Hash)
import PassVeil.Store.Metadata (Metadata)
import PassVeil.Store.Path (Path)
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Cached as Cached
import qualified PassVeil.Store.Content as Content
import qualified PassVeil.Store.Metadata as Metadata

-- | Modifications that have happened to the `Index` due to some outside
-- operating (e.g. pull from a remote repository).
data IndexMod
  = Removed Path
  | Inserted Path
  | Modified Path
  | Updated Path

pathFromIndexMod :: IndexMod -> Path
pathFromIndexMod (Removed path) = path
pathFromIndexMod (Inserted path) = path
pathFromIndexMod (Modified path) = path
pathFromIndexMod (Updated path) = path

-- | The `Index` is used to store unobfuscated `Path` information as well as
-- `Metadata` for `Content` values. It's stored at end points rather than in the
-- repository itself. Data can only be retrieved if one can decrypt the
-- `Content`.
newtype Index = Index
  { fromIndex :: HashMap Path Cached }
  deriving newtype (FromJSON, ToJSON)

-- | Create an empty `Index`.
empty :: Index
empty = Index HashMap.empty


-- | Monad that run `Index` operations in.
type IndexM = StateT (Store, Index) IO

-- | Convert the `Index` to an associative List.
toList :: IndexM [(Path, Cached)]
toList = do
  index <- snd <$> State.get

  return $ HashMap.toList $ fromIndex index

modify :: (HashMap Path Cached -> HashMap Path Cached) ->  IndexM ()
modify f = State.modify' (second (Index . f . fromIndex))

paths :: IndexM [Path]
paths = List.sort . HashMap.keys . fromIndex . snd <$> State.get

insert :: Path -> (Hash, Metadata) -> IndexM ()
insert path (hash, metadata) = do
  store <- fst <$> State.get

  let filePath = Store.filePathForHash store hash

  cached <- Cached hash metadata <$>
    liftIO (Directory.getModificationTime filePath)

  modify (HashMap.insert path cached)

-- | Delete a `Path` from the `Index`.
delete :: Path -> IndexM ()
delete = modify . HashMap.delete

-- | Clear the entire `Index`.
clear :: IndexM ()
clear = State.modify $ second $ const empty

-- | Lookup `Path` in `Index`.
lookup :: Path -> IndexM (Maybe Cached)
lookup path = do
  index <- fromIndex .snd <$> State.get

  return (HashMap.lookup path index)

-- | Update `Index` according to data in the `Store`. This might require to
-- decrypt new or changed data in the store which can lead to user interaction
-- with @gpg@.
refresh :: IndexM [IndexMod]
refresh = do
  (store, index) <- State.get
  hashes <- liftIO (Store.hashes store)

  let whoami = Store.whoami store
      cached = HashMap.toList (fromIndex index)
      new = filter (not . isCached cached) hashes

  oldMods <- forM cached $ \(path, cached') -> do
    let hash = Cached.hash cached'
        filePath = Store.filePathForHash store hash

    exists <- liftIO (Store.doesContentExist store hash whoami)

    if exists
       then do
         modified <- liftIO (Directory.getModificationTime filePath)

         if (modified /= Cached.modified cached')
            then do
              let key = (hash, whoami)

                  update content = do
                    insert' hash content

                    let updated' = Metadata.updated
                          (Cached.metadata cached')

                        updated = Metadata.updated
                          (Content.metadata content)

                    return $ if updated' /= updated
                      then Updated path
                      else Modified path

              liftIO (Store.lookup key store) >>= mapM update

            else return Nothing

        else do
          delete path
          return (Just (Removed path))

  newMods <- forM new $ \hash -> do
    let key = (hash, whoami)

    liftIO (Store.lookup key store) >>= maybe
      (pure Nothing)
      (\content -> insert' hash content >>
        return (Just (Inserted (Content.path content))))

  return $ List.sortOn pathFromIndexMod $ catMaybes $ oldMods ++ newMods
  where
    insert' hash content = insert
      (Content.path content)
      (hash, Content.metadata content)

    isCached cached hash = foldr
      (\(_, c) acc -> acc || Cached.hash c == hash)
      False
      cached

-- | Destroy the `Index` database.
destroy :: Store -> IO ()
destroy store = do
  let path = Store.toIndexPath store

  exists <- Directory.doesFileExist path

  when exists $ Directory.removeFile path
