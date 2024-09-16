module PassVeil.Exit
  ( module System.Exit
  , module PassVeil.Exit
  )
where

import qualified Data.Text as Text

import PassVeil.Store.Identity (Identity)
import PassVeil.Store.Path (Path)
import qualified PassVeil.Console as Console
import qualified PassVeil.Store.Identity as Identity
import qualified PassVeil.Store.Path as Path

import System.Exit
import qualified System.Exit as Exit

die' :: String -> IO a
die' msg = do
  Console.withColor' Console.Vivid Console.Red $
    putStrLn msg

  Exit.exitFailure

notFound :: Path -> IO a
notFound path = do
  let path' = Text.unpack $ Path.fromPath path

  die' $ path' ++ " not found"

alreadyExists :: Path -> IO a
alreadyExists path = do
  let path' = Text.unpack $ Path.fromPath path

  die' $ path' ++ " already exists"

nothingToDo :: IO a
nothingToDo = die' "Nothing to do"

couldNotEdit :: IO a
couldNotEdit = die' "Could not edit"

couldNotRunEditor :: IO a
couldNotRunEditor = die' "Could not run EDITOR"

indexCorrupted :: IO a
indexCorrupted = die' "Index corrupted, aborting"

directoryAlreadyExits :: FilePath -> IO a
directoryAlreadyExits filePath =
  die' $ filePath <> " already exists, aborting"

orphaned :: IO a
orphaned = die' "Will not remove all receivers, aborting"

repositoryDirty :: IO a
repositoryDirty = die' "Repository dirty, aborting"

unknownIdentity :: Identity -> IO a
unknownIdentity identity = die' $
  "Unknown identity: " <> Text.unpack (Identity.fromIdentity identity)

storeError :: IO a
storeError = die' "Could not open store"

encryptFailed :: IO a
encryptFailed = die' "Encryption failed"

signFailed :: IO a
signFailed = die' "Signing failed"

decryptFailed :: IO a
decryptFailed = die' "Decryption failed"

verifyFailed :: IO a
verifyFailed = die' "Verification failed"

suspicionsContent :: IO a
suspicionsContent = die' "Suspicious content, aborting"

corruptedContent :: String -> IO a
corruptedContent err = die' $ "Content corrupted: " <> err
