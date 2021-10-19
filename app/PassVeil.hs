module PassVeil where

import Control.Exception (catch)
import Control.Monad (when, (>=>))
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.IO as Text
import qualified Data.Version as Version
import qualified PassVeil.Console as Console
import qualified PassVeil.Exit as Exit
import PassVeil.Store (Store)
import qualified PassVeil.Store as Store
import PassVeil.Store.Cached (Cached)
import PassVeil.Store.Content (Content)
import PassVeil.Store.Fingerprint (Fingerprint)
import qualified PassVeil.Store.Gpg as Gpg
import PassVeil.Store.Identity (Identity)
import PassVeil.Store.Index (Index, IndexM, IndexMod)
import qualified PassVeil.Store.Index as Index
import PassVeil.Store.Key (Key)
import PassVeil.Store.Path (Path)
import qualified PassVeil.Store.Path as Path
import PassVeil.Store.Payload (Payload)
import qualified PassVeil.Store.Repository as Repository
import qualified Paths_passveil
import qualified System.Directory as Directory

getCached :: Store -> Path -> IO Cached
getCached store path =
  withIndex False store (Index.lookup path)
    >>= maybe
      (Exit.notFound path)
      pure

getContent :: Store -> Path -> Key -> Maybe Payload -> IO Content
getContent store path key mPayload = action `catch` handle
  where
    action =
      Store.lookup key store mPayload
        >>= maybe
          (Exit.notFound path)
          pure

    handle :: Gpg.DecryptException -> IO a
    handle Gpg.DecryptFailed = Exit.decryptFailed
    handle Gpg.VerifyFailed = Exit.verifyFailed
    handle (Gpg.SuspiciousContent _) = Exit.suspicionsContent
    handle (Gpg.CorruptedContent err) = Exit.corruptedContent err

insertContent :: Key -> Content -> Store -> IO ()
insertContent key content store = action `catch` handle
  where
    action = Store.insert key content store

    handle :: Gpg.EncryptException -> IO a
    handle Gpg.EncryptFailed = Exit.encryptFailed
    handle Gpg.SignFailed = Exit.signFailed

repositoryGuard :: Store -> IO ()
repositoryGuard store = do
  dirty <- Repository.isDirty store

  when
    dirty
    Exit.repositoryDirty

whois :: Identity -> IO Fingerprint
whois identity =
  Gpg.whois identity
    >>= maybe
      (Exit.unknownIdentity identity)
      pure

getStore :: Maybe FilePath -> IO Store
getStore = Store.load >=> maybe Exit.storeError pure

withIndex :: Bool -> Store -> IndexM a -> IO a
withIndex update store act = do
  (index, rebuilt) <- readIndex

  (result, updated) <- State.runStateT act (store, index)

  when (update || rebuilt) $
    writeIndex (snd updated)

  return result
  where
    readIndex = do
      let path = Store.toIndexPath store
      exists <- Directory.doesFileExist path

      if exists
        then ByteString.readFile path >>= parseIndex
        else pure (Index.empty, True)

    writeIndex =
      ByteString.writeFile (Store.toIndexPath store) . Aeson.encode

    parseIndex :: ByteString -> IO (Index, Bool)
    parseIndex input = do
      case Aeson.decode input of
        Nothing -> Exit.die "index corrupt"
        Just index -> return (index, False)

printIndexMod :: IndexMod -> IO ()
printIndexMod mod' = case mod' of
  Index.Removed path -> print' Console.Red "- " path
  Index.Inserted path -> print' Console.Green "+ " path
  Index.Modified path -> print' Console.Yellow "* " path
  Index.Updated path -> print' Console.Magenta "! " path
  where
    print' color prefix path = do
      Console.withColor color $
        putStr prefix
      Text.putStrLn (Path.fromPath path)

printVersion :: IO ()
printVersion =
  putStrLn $
    Version.showVersion Paths_passveil.version
