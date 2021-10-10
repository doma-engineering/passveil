module PassVeil.Command.Init
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>), optional)
import Control.Monad (when, forM_)

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import PassVeil.Store.Identity (Identity)
import qualified PassVeil as PassVeil
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Repository as Repository

import qualified System.Directory as Directory

data Options = Options
  { optionsUntracked :: !Bool
  , optionsSigned :: !Bool
  , optionsIdentity :: !Identity
  , optionsRemote :: !(Maybe String)
  }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "Initialize a new store")
  where
    parser = Options
         <$> Options.untrackedFlag
         <*> Options.unsignedFlag
         <*> Options.identityArgument
         <*> optional Options.remoteArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  path <- Store.getDirectory mStore

  exists <- Directory.doesDirectoryExist path
  when exists $
    Exit.directoryAlreadyExits path

  fingerprint <- PassVeil.whois (optionsIdentity options)
  store <- Store.init fingerprint path (optionsSigned options)

  if optionsUntracked options
     then Directory.createDirectoryIfMissing True (Store.toStorePath store)
     else Repository.init store (optionsRemote options)

  Index.destroy store

  mods <- PassVeil.withIndex True store $
    Index.refresh

  forM_ mods PassVeil.printIndexMod
