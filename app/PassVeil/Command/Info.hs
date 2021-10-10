{-# LANGUAGE OverloadedStrings #-}
module PassVeil.Command.Info
  ( Options
  , parse
  , run
  )
where

import Control.Applicative ((<**>))
import Control.Monad (forM_, foldM)

import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Time (TimeZone, UTCTime)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time

import Options.Applicative (ParserInfo)
import qualified Options.Applicative as Options

import PassVeil.Store.Fingerprint (Fingerprint)
import PassVeil.Store.Metadata (Metadata)
import PassVeil.Store.Path (Path)
import PassVeil.Store.Timestamp (Timestamp(Timestamp))
import PassVeil.Store.Uid (Uid)
import qualified PassVeil as PassVeil
import qualified PassVeil.Console as Console
import qualified PassVeil.Options as Options
import qualified PassVeil.Store.Cached as Cached
import qualified PassVeil.Store.Fingerprint as Fingerprint
import qualified PassVeil.Store.Gpg as Gpg
import qualified PassVeil.Store.Log as Log
import qualified PassVeil.Store.Metadata as Metadata
import qualified PassVeil.Store.Timestamp as Timestamp
import qualified PassVeil.Store.Trust as Trust
import qualified PassVeil.Store.Uid as Uid

data Options = Options
  { optionsPath :: !Path }

parse :: ParserInfo Options
parse = Options.info
  (parser <**> Options.helper)
  (Options.progDesc "Show key information")
  where
    parser = Options <$> Options.pathArgument

run :: Maybe FilePath -> Options -> IO ()
run mStore options = do
  store <- PassVeil.getStore mStore

  let path = optionsPath options

  PassVeil.getCached store path >>=
    printMetadata . Cached.metadata

collectFingerprints :: Metadata -> Set Fingerprint
collectFingerprints metadata = Set.unions
  [ Set.singleton (Timestamp.by (Metadata.created metadata))
  , Set.singleton (Timestamp.by (Metadata.issued metadata))
  , Set.fromList (HashMap.keys (Metadata.insiders metadata))
  ]

type Known = HashMap Fingerprint [Uid]

buildKnown :: Known -> Fingerprint -> IO Known
buildKnown acc fingerprint = do
  uids <- Gpg.uids fingerprint

  return (HashMap.insert fingerprint uids acc)

printMetadata :: Metadata -> IO ()
printMetadata metadata = do
  tz <- Time.getCurrentTimeZone
  known <- foldM buildKnown HashMap.empty (collectFingerprints metadata)

  Text.putStrLn "created:"
  printTimestamp tz known (Metadata.created metadata)

  forM_ (Metadata.updated metadata) $ \timestamp -> do
    Text.putStrLn "\nupdated:"
    printTimestamp tz known timestamp

  Text.putStrLn "\nissued:"
  printTimestamp tz known (Metadata.issued metadata)

  Text.putStrLn "\ntrusted:"
  let trusted = List.sortOn (Timestamp.at . snd)
        $ HashMap.toList
        $ Metadata.trusted metadata
   in Console.spacer $ flip map trusted $ \(subject, timestamp) -> do
    printTime tz (Timestamp.at timestamp)
    printFingerprint 3 known subject

  Text.putStrLn "\ninsiders:"
  let insiders = List.sortOn (Timestamp.at . snd)
        $ HashMap.toList
        $ Metadata.insiders metadata
   in Console.spacer $ flip map insiders $ \(subject, timestamp) -> do
        printTime tz (Timestamp.at timestamp)
        printFingerprint 3 known (subject)

  Text.putStrLn "\nlog:"
  Console.spacer $ flip map (Metadata.log metadata) $ \log' -> do
    case log' of
      Log.Allow trust -> do
        printTime tz (Timestamp.at (Trust.timestamp trust))
        printFingerprint 3 known (Timestamp.by (Trust.timestamp trust))

        Console.withColor Console.Green $
          putStr " + "
        printFingerprint 0 known (Trust.subject trust)

      Log.Deny trust -> do
        printTime tz (Timestamp.at (Trust.timestamp trust))
        printFingerprint 3 known (Timestamp.by (Trust.timestamp trust))
        Console.withColor Console.Red $
          putStr " - "
        printFingerprint 0 known (Trust.subject trust)

printTime :: TimeZone -> UTCTime -> IO ()
printTime tz at = do
  Console.withColor Console.Cyan $ do
    let localTime = Time.utcToLocalTime tz at
    putStrLn $ ' ' : Time.formatTime Time.defaultTimeLocale "%c" localTime

printTimestamp :: TimeZone -> Known -> Timestamp -> IO ()
printTimestamp tz known Timestamp{Timestamp.at = at, Timestamp.by = by} = do
  printTime tz at
  printFingerprint 3 known by

printFingerprint :: Int -> Known -> Fingerprint -> IO ()
printFingerprint indent known fingerprint = do
  let uids = fromMaybe [] (HashMap.lookup fingerprint known)

  if null uids
     then printUnknown
     else printKnown uids
  where
    printUnknown = do
      Console.withColor Console.Red $
        Text.putStrLn $ Text.replicate indent " "
          <> Fingerprint.fromFingerprint fingerprint

    printKnown uids = do
      Console.withColor Console.Yellow $
        Text.putStrLn $ Text.replicate indent " "
          <> Fingerprint.fromFingerprint fingerprint

      forM_ uids $ \uid ->
        Console.withColor Console.Green $
          Text.putStrLn $ Text.replicate 5 " " <> Uid.fromUid uid
