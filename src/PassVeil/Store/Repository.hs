{-# LANGUAGE OverloadedStrings #-}
module PassVeil.Store.Repository
  ( init
  , record
  , sync
  , undo
  , isDirty
  )
where

import Prelude hiding (init)

import Control.Monad (when, void)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.FilePath ((</>))
import System.IO (hClose)
import qualified System.Directory as Directory
import qualified System.Process as Process

import PassVeil.Store (Store)
import qualified PassVeil.Store as Store

-- | Check for untracked files and pending rebases.
isDirty :: Store -> IO Bool
isDirty store = do
  repositoryExists <- doesRepositoryExist store

  if repositoryExists
     then do
       let darcs = Process.proc "darcs"
             [ "status"
             , "--no-cache"
             , "--repodir", Store.toStorePath store
             ]
           darcsPipe = darcs
             { Process.std_out = Process.CreatePipe
             , Process.std_err = Process.CreatePipe
             }

       (_ , Just hOut, Just hErr, h) <- Process.createProcess darcsPipe

       dirty <- do
         output <- Text.lines <$> Text.hGetContents hOut
         err <- Text.lines <$> Text.hGetContents hErr

         return $ output /= ["No changes!"] || not (null err)

       hClose hOut
       hClose hErr

       void $ Process.waitForProcess h

       return dirty
     else return False

doesRepositoryExist :: Store -> IO Bool
doesRepositoryExist store = do
  let repository = Store.toStorePath store </> "_darcs"

  Directory.doesDirectoryExist repository

-- | Record a new patch.
record
  :: Store -- ^ `Store` to record in
  -> String -- ^ Patch message
  -> IO ()
record store reason = do
  repositoryExists <- doesRepositoryExist store

  when repositoryExists $ do
    Process.callProcess "darcs" $
      [ "record"
      , "--quiet"
      , "--no-cache"
      , "--repodir", Store.toStorePath store
      , "--look-for-adds"
      , "-a"
      , "-m", reason
      ]

-- | Initialize an new @darcs@ repository.
init :: Store -> Maybe String -> IO ()
init store mRemote = do
  maybe init' clone mRemote
  where
    init' = Process.callProcess "darcs"
      [ "init"
      , "--quiet"
      , "--no-cache"
      , Store.toStorePath store
      ]

    clone remote = Process.callProcess "darcs"
      [ "clone"
      , "--quiet"
      , "--lazy"
      , "--no-cache"
      , "--set-default"
      , remote
      , Store.toStorePath store
      ]

-- | Obliterate unpushed patches.
undo :: Store -> IO ()
undo store =  do
  repositoryExists <- doesRepositoryExist store

  when repositoryExists $ do
    remoteExists <- hasRemote store

    if remoteExists
       then Process.callProcess "darcs"
         [ "obliterate"
         , "--no-cache"
         , "--quiet"
         , "--not-in-remote"
         , "--repodir", Store.toStorePath store
         ]
       else Process.callProcess "darcs"
         [ "obliterate"
         , "--no-cache"
         , "--quiet"
         , "--repodir", Store.toStorePath store
         ]

-- | Pull patches from remote repository. Local conflicting patches get rebased
-- during the pull.
sync :: Store -> IO ()
sync store = do
  repositoryExists <- doesRepositoryExist store

  when repositoryExists $ do
    remoteExists <- hasRemote store

    when remoteExists $ do
      Process.callProcess "darcs"
        [ "rebase"
        , "pull"
        , "--no-cache"
        , "--quiet"
        , "-a"
        , "--repodir", Store.toStorePath store
        ]

      Process.callProcess "darcs"
        [ "push"
        , "--no-cache"
        , "--quiet"
        , "-a"
        , "--repodir", Store.toStorePath store
        ]

hasRemote :: Store -> IO Bool
hasRemote store = do
  let remote = Store.toStorePath store
           </> "_darcs"
           </> "prefs"
           </> "defaultrepo"

  Directory.doesFileExist remote
