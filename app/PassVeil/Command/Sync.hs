module PassVeil.Command.Sync
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>))
import Control.Monad (forM_, when, unless)

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import qualified PassVeil
import qualified PassVeil.Options as Options
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Repository as Repository

data Options = Options
  { optionsReindex :: !Bool
  , optionsOffline :: !Bool
  }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "Synchronize store")
  where
    parser = Options
         <$> Options.reindexFlag
         <*> Options.offlineFlag

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  PassVeil.repositoryGuard store

  unless (optionsOffline options) $
    Repository.sync store

  mods <- PassVeil.withIndex True store $ do
    when (optionsReindex options) Index.clear

    Index.refresh

  forM_ mods PassVeil.printIndexMod
