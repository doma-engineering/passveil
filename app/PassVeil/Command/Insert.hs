module PassVeil.Command.Insert
  ( Options
  , parse
  , run
  )
where

import Control.Monad (when, forM_)
import Control.Applicative ((<**>), optional)

import Data.Bool (bool)
import qualified Data.Text.IO as Text

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import PassVeil.Store (Store)
import PassVeil.Store.Fingerprint (Fingerprint)
import PassVeil.Store.Generator (Generator)
import PassVeil.Store.Hash (Hash)
import PassVeil.Store.Path (Path)
import PassVeil.Store.Payload (Payload)
import qualified PassVeil as PassVeil
import qualified PassVeil.Editor as Editor
import qualified PassVeil.Exit as Exit
import qualified PassVeil.Options as Options
import qualified PassVeil.Store as Store
import qualified PassVeil.Store.Content as Content
import qualified PassVeil.Store.Generator as Generator
import qualified PassVeil.Store.Hash as Hash
import qualified PassVeil.Store.Index as Index
import qualified PassVeil.Store.Repository as Repository

data Options = Options
  { optionsPath :: !Path
  , optionsGenerator :: !(Maybe Generator)
  , optionsBatchmode :: !Bool
  }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "Insert a new password into the store")
  where
    parser = Options
         <$> Options.pathArgument
         <*> optional (Options.generateOption)
         <*> Options.batchFlag

record
  :: Store
  -> Path
  -> Fingerprint
  -> Hash
  -> Payload
  -> IO ()
record store path fingerprint hash payload = do
  content <- Content.create path fingerprint payload

  let key = (hash, fingerprint)

  PassVeil.insertContent key content store

  PassVeil.withIndex True store $
    Index.insert path (hash, Content.metadata content)

  Repository.record store "insert"

runEditor
  :: Store
  -> Path
  -> Fingerprint
  -> Hash
  -> Maybe Payload
  -> IO ()
runEditor store path fingerprint hash mPayload =
  Editor.edit mPayload $
    record store path fingerprint hash

runBatch
  :: Store
  -> Path
  -> Fingerprint
  -> Hash
  -> Maybe Payload
  -> IO ()
runBatch store path fingerprint hash Nothing =
  Text.getLine >>= record store path fingerprint hash

runBatch store path fingerprint hash mPayload =
  forM_ mPayload $
    record store path fingerprint hash

run  :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  PassVeil.repositoryGuard store

  let path = optionsPath options
      hash = Hash.compute path
      mGenerator = optionsGenerator options

  exists <- Store.member hash store

  when exists $
    Exit.alreadyExists path

  traverse Generator.generate mGenerator >>=
    let batchmode = optionsBatchmode options
        run' = bool runEditor runBatch batchmode
        fingerprint = Store.whoami store
     in run' store path fingerprint hash
