{-# LANGUAGE ScopedTypeVariables #-}
module PassVeil.Options where

import Text.Regex.PCRE (Regex)
import qualified Text.Regex.PCRE as Regex

import Options.Applicative (Parser)
import qualified Options.Applicative as Options

import PassVeil.Store.Generator (Generator)
import PassVeil.Store.Identity (Identity(Identity))
import PassVeil.Store.Path (Path)
import qualified PassVeil.Store.Generator as Generator
import qualified PassVeil.Store.Path as Path

pathArgument :: Parser Path
pathArgument = Options.argument parse $ Options.metavar "PATH"
  where
    parse =
      maybe (fail "invalid path") pure . Path.parse =<< Options.str

regexArgument :: Parser Regex
regexArgument = Options.argument parse $
  Options.metavar "REGEX"
  where
    parse = do
      (format :: String) <- Options.str
      Regex.makeRegexM format

identityArgument :: Parser Identity
identityArgument = Identity <$>
  Options.strArgument (Options.metavar "IDENTITY")

remoteArgument :: Parser String
remoteArgument = Options.strArgument (Options.metavar "REMOTE")

treeFlag :: Parser Bool
treeFlag = Options.switch $
     Options.help "Use tree format"
  <> Options.long "tree"

untrackedFlag :: Parser Bool
untrackedFlag = Options.switch $
     Options.help "Do not track store with version control"
  <> Options.long "untracked"

unsignedFlag :: Parser Bool
unsignedFlag = fmap not $ Options.switch $
     Options.help "Do not sign keys"
  <> Options.long "unsigned"

unverifiedFlag :: Parser Bool
unverifiedFlag = Options.switch $
     Options.help "Do not verify data"
  <> Options.long "unverified"

reindexFlag :: Parser Bool
reindexFlag = Options.switch $
     Options.help "Force index rebuild"
  <> Options.long "reindex"

offlineFlag :: Parser Bool
offlineFlag = Options.switch $
     Options.help "Do not synchronize with remote repository"
  <> Options.long "offline"

batchFlag :: Parser Bool
batchFlag = Options.switch $
     Options.help "Use batch mode"
  <> Options.long "batch"

generateOption :: Parser Generator
generateOption = Options.option parse $
     Options.help "Generator rules"
  <> Options.long "generate"
  <> Options.metavar "RULES"
  where
    parse = Options.str >>=
      either fail return . Generator.parse

storeOption :: Parser FilePath
storeOption = Options.strOption $
     Options.help "Specify an alternate store"
  <> Options.long "store"
  <> Options.metavar "DIRECTORY"

versionFlag :: Parser ()
versionFlag = Options.flag' () $
     Options.help "Show version and exit"
  <> Options.long "version"
  <> Options.hidden
