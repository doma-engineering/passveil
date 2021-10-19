module PassVeil.Command.Deny
  ( Options,
    parse,
    run,
  )
where

import Control.Applicative (some, (<**>))
import Control.Monad (forM_, when)
import qualified Data.HashMap.Strict as HashMap
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options
import qualified PassVeil as PassVeil
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
    (Options.progDesc "Deny password to be shared with others")
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

  (deleted, metadata) <- do
    let identities = optionsIdentities options

    denied <- traverse PassVeil.whois identities
    cached <- PassVeil.getCached store path
    timestamp <- Timestamp.new whoami

    let metadata = Cached.metadata cached

    maybe
      Exit.nothingToDo
      pure
      (Metadata.deny timestamp denied metadata)

  let receivers = HashMap.keys (Metadata.trusted metadata)

  when (null receivers) $
    Exit.orphaned

  let hash = Hash.compute path
      key = (hash, whoami)

  content <-
    Content.touch metadata
      <$> PassVeil.getContent store path key Nothing

  forM_ deleted $ \fingerprint ->
    Store.delete (hash, fingerprint) store

  forM_ receivers $ \receiver ->
    PassVeil.insertContent
      (hash, receiver)
      content
      store

  PassVeil.withIndex True store $ do
    if whoami `elem` receivers
      then Index.insert path (hash, metadata)
      else Index.delete path

  Repository.record store "deny"
