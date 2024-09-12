module Main where

import Prelude hiding (Show)

import Control.Applicative ((<**>), (<|>), optional)

import qualified Options.Applicative as Options

import qualified PassVeil (printVersion)
import qualified PassVeil.Command.Allow as Allow
import qualified PassVeil.Command.Copy as Copy
import qualified PassVeil.Command.Delete as Delete
import qualified PassVeil.Command.Deny as Deny
import qualified PassVeil.Command.Distrust as Distrust
import qualified PassVeil.Command.Edit as Edit
import qualified PassVeil.Command.Info as Info
import qualified PassVeil.Command.Init as Init
import qualified PassVeil.Command.Insert as Insert
import qualified PassVeil.Command.List as List
import qualified PassVeil.Command.Move as Move
import qualified PassVeil.Command.Search as Search
import qualified PassVeil.Command.Show as Show
import qualified PassVeil.Command.Sync as Sync
import qualified PassVeil.Command.Undo as Undo
import qualified PassVeil.Options as Options

import System.Posix.Types (FileMode)
import qualified System.Posix.Files as Files

data Options
  = ShowVersion
  | Options !(Maybe FilePath) !SubCommandOptions

data SubCommandOptions
  = Allow Allow.Options
  | Copy Copy.Options
  | Delete Delete.Options
  | Deny Deny.Options
  | Distrust Distrust.Options
  | Edit Edit.Options
  | Info Info.Options
  | Init Init.Options
  | Insert Insert.Options
  | List List.Options
  | Move Move.Options
  | Search Search.Options
  | Show Show.Options
  | Sync Sync.Options
  | Undo Undo.Options

permissions :: FileMode
permissions = Files.unionFileModes
  Files.groupModes
  Files.otherModes

run :: Maybe FilePath -> SubCommandOptions -> IO ()
run mStore subCommandOptions = case subCommandOptions of
  Allow options -> Allow.run mStore options
  Copy options -> Copy.run mStore options
  Deny options -> Deny.run mStore options
  Distrust options -> Distrust.run mStore options
  Info options -> Info.run mStore options
  Insert options -> Insert.run mStore options
  Delete options -> Delete.run mStore options
  Edit options -> Edit.run mStore options
  Move options -> Move.run mStore options
  Show options -> Show.run mStore options
  List options -> List.run mStore options
  Search options -> Search.run mStore options
  Init options -> Init.run mStore options
  Sync options -> Sync.run mStore options
  Undo options -> Undo.run mStore options

main :: IO ()
main = do
  _ <- Files.setFileCreationMask permissions

  command <- Options.execParser $ Options.info
    (parser <**> Options.helper)
    (Options.progDesc "passveil - distributed password manager")

  case command of
    Options mStore options -> run mStore options
    ShowVersion -> PassVeil.printVersion
  where
    keyManagement = Options.subparser $
         Options.commandGroup "Key management:"
      <> Options.command "insert" (Insert <$> Insert.parse)
      <> Options.command "delete" (Delete <$> Delete.parse)
      <> Options.command "edit" (Edit <$> Edit.parse)
      <> Options.command "move" (Move <$> Move.parse)
      <> Options.command "copy" (Copy <$> Copy.parse)

    queryOperations = Options.subparser $
         Options.commandGroup "Query operations:"
      <> Options.command "show" (Show <$> Show.parse)
      <> Options.command "list" (List <$> List.parse)
      <> Options.command "search" (Search <$> Search.parse)
      <> Options.command "info" (Info <$> Info.parse)
      <> Options.hidden

    trustManagement = Options.subparser $
         Options.commandGroup "Trust management:"
      <> Options.command "allow" (Allow <$> Allow.parse)
      <> Options.command "deny" (Deny <$> Deny.parse)
      <> Options.command "distrust" (Distrust <$> Distrust.parse)
      <> Options.hidden

    storeManagement = Options.subparser $
         Options.commandGroup "Storage management:"
      <> Options.command "init" (Init <$> Init.parse)
      <> Options.command "sync" (Sync <$> Sync.parse)
      <> Options.command "undo" (Undo <$> Undo.parse)
      <> Options.hidden

    commands = keyManagement
           <|> queryOperations
           <|> trustManagement
           <|> storeManagement

    versionParser = Options.versionFlag *> pure ShowVersion
    optionsParser = Options <$> optional Options.storeOption <*> commands

    parser = optionsParser <|> versionParser
