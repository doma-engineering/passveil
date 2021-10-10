{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module PassVeil.Store.Path
  ( Path
  , fromPath
  , explode
  , isPrefixOf
  , parse
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

-- | A `Path` is a human-readble key for a `Content` value.
newtype Path = Path { fromPath :: Text }
  deriving newtype
    ( Eq
    , FromJSON
    , FromJSONKey
    , Hashable
    , Ord
    , Show
    , ToJSON
    , ToJSONKey
    )

-- | Explode `Path` into a non-empty list. The `Path` is split up at @\'/\'@
-- characters.
explode :: Path -> NonEmpty Text
explode = NonEmpty.fromList . split . fromPath

-- | Parse an arbitrary `Text` into a `Path`. This removes leading and duplicate
-- @\'/\'@ characters.
parse :: Text -> Maybe Path
parse input
  | derooted <- Text.dropWhile (== '/') input
  , not (Text.null derooted)
  , exploded <- filter (not . Text.null) (split derooted)
  , not (null exploded) =
    Just (Path (Text.intercalate "/" exploded))

  | otherwise = Nothing

-- | Check if a `Path` is a prefix of another `Path`.
isPrefixOf :: Path -> Path -> Bool
isPrefixOf (Path prefix) (Path path) =
  split prefix `List.isPrefixOf` split path

split :: Text -> [Text]
split = Text.splitOn "/"
