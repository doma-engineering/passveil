module PassVeil.Command.Distrust
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>), some)
import Control.Monad (forM_)

import Data.List (intersect, sort)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as Text

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import PassVeil.Store.Identity (Identity)
import qualified PassVeil as PassVeil
import qualified PassVeil.Options as Options
import qualified PassVeil.Store.Cached as Cached
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Metadata as Metadata
import qualified PassVeil.Store.Path as Path

data Options = Options
  { optionsIdentities :: ![Identity] }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "List potentially compromised passwords")
  where
    parser = Options <$> some Options.identityArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  let identities = optionsIdentities options

  distrusted <- traverse PassVeil.whois identities
  entries <- PassVeil.withIndex False store $
    Index.toList

  let tainted = sort
        $ map fst
        $ filter (isTainted distrusted . snd) entries

  forM_ tainted $
    Text.putStrLn . Path.fromPath
  where
    isTainted distrusted cached =
      let metadata = Cached.metadata cached
          insiders = HashMap.keys (Metadata.insiders metadata)
       in not $ null $ distrusted `intersect` insiders
