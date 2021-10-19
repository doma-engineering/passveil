module PassVeil.Command.Show
  ( Options,
    parse,
    run,
  )
where

import Control.Applicative ((<**>))
import qualified Data.Text.IO as Text
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options
import qualified PassVeil as PassVeil
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Content as Content
import qualified PassVeil.Store.Hash as Hash
import PassVeil.Store.Path (Path)

data Options = Options
  { optionsPath :: !Path,
    optionsBatchMode :: !Bool
  }

parse :: ParserInfo Options
parse =
  Options.info
    (parser <**> Options.helper)
    (Options.progDesc "Show password of a path")
  where
    parser = Options <$> Options.pathArgument <*> Options.batchFlag

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  let path = optionsPath options
      fingerprint = Store.whoami store
      key = (Hash.compute path, fingerprint)

  PassVeil.getContent store path key
    >>= Text.putStrLn . Content.payload
