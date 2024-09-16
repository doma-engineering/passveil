module PassVeil.Command.Copy
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>))
import Control.Monad (when)

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import PassVeil.Store.Path (Path)
import qualified PassVeil
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Content as Content
import qualified PassVeil.Store.Hash as Hash
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Repository as Repository

data Options = Options
  { optionsSource :: !Path
  , optionsTarget :: !Path
  }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "Copy a password to another path")
  where
    parser = Options
         <$> Options.pathArgument
         <*> Options.pathArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  PassVeil.repositoryGuard store

  let fingerprint = Store.whoami store

      source = optionsSource options
      target = optionsTarget options

      sourceHash = Hash.compute source
      targetHash = Hash.compute target

      sourceKey = (sourceHash, fingerprint)
      targetKey = (targetHash, fingerprint)

  exists <- Store.member targetHash store

  when exists $
    Exit.alreadyExists target

  content <- PassVeil.getContent store source sourceKey Nothing

  let updated = content { Content.path = target }

  PassVeil.insertContent targetKey updated store

  PassVeil.withIndex True store $ do
    let metadata = Content.metadata updated

    Index.insert target (targetHash, metadata)

  Repository.record store "copy"
