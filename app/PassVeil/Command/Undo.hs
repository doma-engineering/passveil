module PassVeil.Command.Undo
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>))
import Control.Monad (forM_)

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import qualified PassVeil as PassVeil
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Repository as Repository

data Options = Options

parse :: ParserInfo Options
parse = Options.info
  (pure Options <**> Options.helper)
  (Options.progDesc "Undo local changes")

run :: Maybe FilePath -> Options -> IO ()
run mStore _ = do
  store <- PassVeil.getStore mStore

  Repository.undo store

  mods <- PassVeil.withIndex True store $
    Index.refresh

  forM_ mods PassVeil.printIndexMod
