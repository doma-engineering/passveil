module PassVeil.Command.Show
  ( Options,
    parse,
    run,
  )
where

import Control.Applicative ((<**>))
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options
import qualified PassVeil
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Content as Content
import qualified PassVeil.Store.Hash as Hash
import PassVeil.Store.Path (Path)

--import System.Console.Haskeline (defaultSettings, getPassword, runInputT)

data Options = Options
  { optionsPath :: !Path,
    optionsBatchMode :: !Bool,
    optionsUnverified :: !Bool
  }

parse :: ParserInfo Options
parse =
  Options.info
    (parser <**> Options.helper)
    (Options.progDesc "Show password of a path")
  where
    parser = Options
      <$> Options.pathArgument
      <*> Options.batchFlag
      <*> Options.unverifiedFlag

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- getStore

  let path = optionsPath options
      fingerprint = Store.whoami store
      key = (Hash.compute path, fingerprint)
      isPayloadRequired = if optionsBatchMode options then Just "yes" else Nothing

  -- mPayload <-
  --   if isPayloadRequired
  --     then runInputT defaultSettings $ getPassword (Just '*') "(running in batch mode): Enter your GPG2 key password> "
  --     else pure Nothing

  PassVeil.getContent store path key (T.pack <$> isPayloadRequired)
    >>= Text.putStrLn . Content.payload

  where
      getStore = do
        store <- PassVeil.getStore mStore

        return $ if optionsUnverified options
          then store { Store.signed = False }
          else store
