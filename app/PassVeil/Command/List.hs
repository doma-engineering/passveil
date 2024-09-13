module PassVeil.Command.List
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>), optional)
import Control.Monad (forM_)

import qualified Data.Text.IO as Text

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import PassVeil.Store.Path (Path)
import qualified PassVeil
import qualified PassVeil.Options as Options
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Path as Path
import qualified PassVeil.Tree as Tree

data Options = Options
  { optionsTree :: !Bool
  , optionsPath :: !(Maybe Path)
  }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "List all passwords below a path")
  where
    parser = Options
         <$> Options.treeFlag
         <*> optional Options.pathArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  paths <- filter relevant <$>
    PassVeil.withIndex False store Index.paths

  if optionsTree options
     then Tree.draw paths
     else forM_ paths (Text.putStrLn . Path.fromPath)
  where
    relevant path' = case optionsPath options of
      Just prefix -> prefix `Path.isPrefixOf` path'
      Nothing -> True
