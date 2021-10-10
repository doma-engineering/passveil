module PassVeil.Command.Delete
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>))

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import System.FilePath ((</>))
import qualified System.Directory as Directory

import PassVeil.Store.Path (Path)
import qualified PassVeil as PassVeil
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Hash as Hash
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Repository as Repository

data Options = Options
  { optionsPath :: !Path }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "Delete a password from the store")
  where
    parser = Options <$> Options.pathArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  PassVeil.repositoryGuard store

  let path = optionsPath options
      whoami = Store.whoami store
      hash = Hash.compute path

  exists <- Store.doesContentExist store hash whoami

  if exists
     then do
       let store' = Store.toStorePath store
           hash' = Hash.toFilePath hash
           dir = store' </> hash'

       Directory.removeDirectoryRecursive dir

       PassVeil.withIndex True store $
         Index.delete path

       Repository.record store "delete"
     else
       Exit.notFound path
