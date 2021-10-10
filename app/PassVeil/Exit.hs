module PassVeil.Exit
  ( module System.Exit
  , module PassVeil.Exit
  )
where

import qualified Data.Text as Text

import PassVeil.Store.Identity (Identity)
import PassVeil.Store.Path (Path)
import qualified PassVeil.Store.Identity as Identity
import qualified PassVeil.Store.Path as Path

import System.Exit
import qualified System.Exit as Exit

notFound :: Path -> IO a
notFound path = do
  let path' = Text.unpack $ Path.fromPath path

  Exit.die $ path' ++ " not found"

alreadyExists :: Path -> IO a
alreadyExists path = do
  let path' = Text.unpack $ Path.fromPath path

  Exit.die $ path' ++ " already exists"

nothingToDo :: IO a
nothingToDo = Exit.die "Nothing to do"

couldNotEdit :: IO a
couldNotEdit = Exit.die "Could not edit"

couldNotRunEditor :: IO a
couldNotRunEditor = Exit.die "Could not run EDITOR"

indexCorrupted :: IO a
indexCorrupted = Exit.die "Index corrupted, aborting"

directoryAlreadyExits :: FilePath -> IO a
directoryAlreadyExits filePath =
  Exit.die $ filePath <> " already exists, aborting"

orphaned :: IO a
orphaned = Exit.die "Will not remove all receivers, aborting"

repositoryDirty :: IO a
repositoryDirty = Exit.die "Repository dirty, aborting"

unknownIdentity :: Identity -> IO a
unknownIdentity identity = Exit.die $
  "Unkown identity: " <> Text.unpack (Identity.fromIdentity identity)

storeError :: IO a
storeError = Exit.die "Could not open store"

encryptFailed :: IO a
encryptFailed = Exit.die "Encryption failed"

signFailed :: IO a
signFailed = Exit.die "Signing failed"

decryptFailed :: IO a
decryptFailed = Exit.die "Decryption failed"

verifyFailed :: IO a
verifyFailed = Exit.die "Verification failed"

suspicionsContent :: IO a
suspicionsContent = Exit.die "Suspicious content, aborting"

corruptedContent :: String -> IO a
corruptedContent err = Exit.die $ "Content corrupted: " <> err
