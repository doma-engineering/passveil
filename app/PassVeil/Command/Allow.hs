module PassVeil.Command.Allow
  ( Options,
    parse,
    run,
  )
where

import Control.Applicative (some, (<**>))
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HashMap
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options
import qualified PassVeil
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Cached as Cached
import qualified PassVeil.Store.Content as Content
import qualified PassVeil.Store.Hash as Hash
import PassVeil.Store.Identity (Identity)
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Metadata as Metadata
import PassVeil.Store.Path (Path)
import qualified PassVeil.Store.Repository as Repository
import qualified PassVeil.Store.Timestamp as Timestamp

data Options = Options
  { optionsPath :: !Path,
    optionsIdentities :: ![Identity]
  }

parse :: ParserInfo Options
parse =
  Options.info
    (parser <**> Options.helper)
    (Options.progDesc "Allow password to be shared with others")
  where
    parser =
      Options
        <$> Options.pathArgument
        <*> some Options.identityArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  PassVeil.repositoryGuard store

  let whoami = Store.whoami store
      path = optionsPath options

  metadata <- do
    let identities = optionsIdentities options

    allowed <- traverse PassVeil.whois identities
    cached <- PassVeil.getCached store path
    timestamp <- Timestamp.new whoami

    let metadata = Cached.metadata cached

    maybe
      Exit.nothingToDo
      pure
      (Metadata.allow timestamp allowed metadata)

  let receivers = HashMap.keys (Metadata.trusted metadata)
      hash = Hash.compute path
      key = (hash, whoami)

  content <-
    Content.touch metadata
      <$> PassVeil.getContent store path key Nothing

  forM_ receivers $ \receiver ->
    PassVeil.insertContent (hash, receiver) content store

  PassVeil.withIndex True store $ do
    Index.insert path (hash, metadata)

  Repository.record store "allow"
