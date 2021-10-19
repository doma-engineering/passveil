{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module PassVeil.Store.Gpg
  ( -- * Encryption
    EncryptException (..),
    encrypt,

    -- * Decryption
    DecryptException (..),
    decrypt,

    -- * Queries
    whois,
    uids,
  )
where

import Control.Exception
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Data.Text (Text, isInfixOf, isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO as Text
import PassVeil.Store.Content (Content)
import qualified PassVeil.Store.Content as Content
import PassVeil.Store.Fingerprint (Fingerprint (Fingerprint))
import qualified PassVeil.Store.Fingerprint as Fingerprint
import qualified PassVeil.Store.Hash as Hash
import PassVeil.Store.Identity (Identity)
import qualified PassVeil.Store.Identity as Identity
import PassVeil.Store.Key
import qualified PassVeil.Store.Metadata as Metadata
import PassVeil.Store.Payload (Payload)
import qualified PassVeil.Store.Timestamp as Timestamp
import PassVeil.Store.Uid (Uid (Uid))
import qualified System.Directory as Directory
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((<.>), (</>))
import System.IO (hClose)
import qualified System.Process as Process

data EncryptException
  = -- | @gpg@ encrypt failed
    EncryptFailed
  | -- | @gpg@ sign failed
    SignFailed
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Encrypt `Content` using public keys of the receivers. Uses the @gpg@
-- command line tools to encrypt `Content`. This function can throw an
-- `EncryptException` when the @gpg@ command fails.
--
-- With signing this function will create a detached signature to assure that
-- the `Content` has been written using the stated private key.
encrypt ::
  Bool -> -- ° Signed encryption

  -- | Filepath to `Store`
  FilePath ->
  -- | `Fingerprint` of encrypter
  Fingerprint ->
  -- | `Key` to store `Content`
  Key ->
  -- | `Content` to encrypt
  Content ->
  IO ()
encrypt signed store whoami (hash, fpr) content =
  gpgEncrypt >> when signed gpgSign
  where
    hash' = Hash.toFilePath hash
    fpr' = Fingerprint.toFilePath fpr
    whoami' = Fingerprint.toFilePath whoami
    path = store </> hash' </> fpr'

    gpgEncrypt = do
      let gpg =
            Process.proc
              "gpg"
              [ "--encrypt",
                "--yes",
                "--armor",
                "--default-key",
                whoami',
                "-o",
                path,
                "-r",
                fpr'
              ]
          gpgPipe = gpg {Process.std_in = Process.CreatePipe}

      (Just hIn, _, _, h) <- Process.createProcess gpgPipe

      ByteString.hPutStr hIn (Aeson.encode content)
      hClose hIn

      exitCode <- Process.waitForProcess h

      when (exitCode /= ExitSuccess) $
        throwIO EncryptFailed

    gpgSign = do
      h <-
        Process.spawnProcess
          "gpg"
          [ "--sign",
            "--yes",
            "--armor",
            "--default-key",
            whoami',
            "--quiet",
            "--detach-sig",
            "-o",
            path <.> "sig",
            path
          ]

      exitCode <- Process.waitForProcess h

      when (exitCode /= ExitSuccess) $
        throwIO SignFailed

data DecryptException
  = DecryptFailed
  | VerifyFailed
  | SuspiciousContent Content
  | CorruptedContent String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Decrypt `Content` using @gpg@ command line tools. This function can throw a
-- `DecryptException` when the @gpg@ command fails.
--
-- With signing this function tries to verify the key that has been used to
-- write the `Content.
decrypt ::
  -- | Verify decryption
  Bool ->
  -- | Filepath to `Store`
  FilePath ->
  -- | `Fingerprint` of decrypter
  Fingerprint ->
  -- | `Key` to look up
  Key ->
  -- | `Nothing` if not in batch mode, otherwise, batch payload
  Maybe Payload ->
  IO (Maybe Content)
decrypt signed store whoami (hash, fpr) mPayload = do
  exists <- Directory.doesFileExist path

  if exists
    then Just <$> bool gpgDecrypt (gpgDecrypt >>= gpgVerify) signed
    else return Nothing
  where
    hash' = Hash.toFilePath hash
    whoami' = Fingerprint.toFilePath whoami
    fpr' = Fingerprint.toFilePath fpr
    path = store </> hash' </> fpr'

    gpgDecrypt = do
      let obligatoryGpgArgs = ["--decrypt", "--default-key", whoami', "--quiet"]
      let optionalGpgArgs =
            if isJust mPayload
              then ["--pinentry-mode", "loopback"]
              else []
      let gpgArgs = obligatoryGpgArgs ++ optionalGpgArgs ++ [path]
      let gpg =
            Process.proc
              "gpg"
              gpgArgs
          gpgPipe = gpg {Process.std_out = Process.CreatePipe, Process.std_in = Process.CreatePipe}

      (Just hin, Just hOut, _, h) <- Process.createProcess gpgPipe
      _ <-
        maybe (pure ()) (TIO.hPutStrLn hin) mPayload

      exitCode <- Process.waitForProcess h

      when (exitCode /= ExitSuccess) $
        throwIO DecryptFailed

      ByteString.hGetContents hOut
        >>= either
          (throwIO . CorruptedContent)
          pure
          . Aeson.eitherDecode

    gpgVerify content = do
      let metadata = Content.metadata content

          issuer =
            Timestamp.by $
              Metadata.issued metadata

      mVerified <- verify whoami path

      case mVerified of
        Nothing -> throwIO VerifyFailed
        Just verified -> do
          let valid =
                issuer == verified
                  && HashMap.member issuer (Metadata.insiders metadata)

          unless valid $
            throwIO (SuspiciousContent content)

          return content

fingerprint :: Text -> IO (Maybe Text)
fingerprint input = do
  let gpg =
        Process.proc
          "gpg"
          [ "--with-colons",
            "--fingerprint",
            Text.unpack input
          ]

      cmd = gpg {Process.std_out = Process.CreatePipe}

  (_, Just hOut, _, h) <- Process.createProcess cmd

  output <- Text.hGetContents hOut

  exitCode <- Process.waitForProcess h

  return $
    if exitCode /= ExitSuccess
      then Nothing
      else Just output

-- | Returns `Fingerprint` for `Identity`.
whois :: Identity -> IO (Maybe Fingerprint)
whois identity = do
  let gpg =
        Process.proc
          "gpg"
          [ "--with-colons",
            "--fingerprint",
            Text.unpack (Identity.fromIdentity identity)
          ]

      cmd = gpg {Process.std_out = Process.CreatePipe}

  (_, Just hOut, _, h) <- Process.createProcess cmd

  fprs <- searchFingerprint <$> Text.hGetContents hOut
  exitCode <- Process.waitForProcess h

  if exitCode /= ExitSuccess
    then return Nothing
    else case fprs of
      fpr : _ -> return (Just fpr)
      [] -> return Nothing
  where
    searchFingerprint contents =
      map (Fingerprint . (!! 9) . Text.splitOn ":") $
        take 1 $
          filter ("fpr" `isPrefixOf`) $
            Text.lines contents

-- | Returns all known `Uid`s for a `Fingerprint`.
uids :: Fingerprint -> IO [Uid]
uids fpr' =
  fmap
    (maybe [] searchUids)
    (fingerprint (Fingerprint.fromFingerprint fpr'))
  where
    searchUids :: Text -> [Uid]
    searchUids contents =
      map (Uid . (!! 9) . Text.splitOn ":") $
        filter ("uid" `isPrefixOf`) $
          Text.lines contents

verify :: Fingerprint -> FilePath -> IO (Maybe Fingerprint)
verify whoami' path = do
  let gpg =
        Process.proc
          "gpg"
          [ "--default-key",
            Fingerprint.toFilePath whoami',
            "--quiet",
            "--verify",
            path <.> "sig",
            path
          ]
      gpgPipe = gpg {Process.std_err = Process.CreatePipe}

  (_, _, Just hOut, h) <- Process.createProcess gpgPipe

  mIssuer <- getIssuer <$> Text.hGetContents hOut
  exitCode <- Process.waitForProcess h

  when (exitCode /= ExitSuccess) $
    throwIO VerifyFailed

  return mIssuer
  where
    getIssuer input =
      let k = filter ("using RSA key" `isInfixOf`) (Text.lines input)
       in case k of
            [] -> Nothing
            key : _ -> Just (Fingerprint (Text.drop 34 key))
