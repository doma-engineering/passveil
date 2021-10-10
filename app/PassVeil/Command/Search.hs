module PassVeil.Command.Search
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>))
import Control.Monad (forM_, when)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Text.Regex.PCRE (matchTest, Regex)

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import qualified PassVeil as PassVeil
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Path as Path
import qualified PassVeil.Tree as Tree

data Options = Options
  { optionsTree :: !Bool
  , optionsRegex :: !Regex
  }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "List passwords matching a regular expression")
  where
    parser = Options
         <$> Options.treeFlag
         <*> Options.regexArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  paths <- filter match' <$>
    PassVeil.withIndex False store Index.paths

  when (null paths) Exit.exitFailure

  if optionsTree options
     then Tree.draw paths
     else forM_ paths (Text.putStrLn . Path.fromPath)
  where
    match' = matchTest (optionsRegex options) . Text.unpack . Path.fromPath
