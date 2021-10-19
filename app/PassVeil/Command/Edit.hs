module PassVeil.Command.Edit
  ( Options,
    parse,
    run,
  )
where

import Control.Applicative (optional, (<**>), (<|>))
import Control.Monad (forM_, when)
import qualified Data.HashMap.Strict as HashMap
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options
import qualified PassVeil as PassVeil
import qualified PassVeil.Editor as Editor
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Content as Content
import PassVeil.Store.Generator (Generator)
import qualified PassVeil.Store.Generator as Generator
import qualified PassVeil.Store.Hash as Hash
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Metadata as Metadata
import PassVeil.Store.Path (Path)
import qualified PassVeil.Store.Repository as Repository

data Options = Options
  { optionsPath :: !Path,
    optionsGenerator :: !(Maybe Generator)
  }

parse :: ParserInfo Options
parse =
  Options.info
    (parser <**> Options.helper)
    (Options.progDesc "Edit an existing password in the store")
  where
    parser =
      Options
        <$> Options.pathArgument
        <*> optional Options.generateOption

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  PassVeil.repositoryGuard store

  let path = optionsPath options
      mGenerator = optionsGenerator options
      whoami = Store.whoami store
      hash = Hash.compute path
      key = (hash, whoami)

  content <- PassVeil.getContent store path key Nothing
  mGenerated <- traverse Generator.generate mGenerator

  let payload = Content.payload content

  Editor.edit (mGenerated <|> Just payload) $ \secret -> do
    when (secret == payload) $
      Exit.nothingToDo

    updated <- Content.update whoami secret content

    let metadata = Content.metadata updated
        receivers = HashMap.keys (Metadata.trusted metadata)

    forM_ receivers $ \receiver -> do
      PassVeil.insertContent (hash, receiver) updated store

    PassVeil.withIndex True store $ do
      Index.insert path (hash, metadata)

    Repository.record store "edit"
