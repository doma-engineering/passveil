{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module PassVeil.Tree (draw) where

import Data.Bool (bool)
import Data.List (groupBy)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Tree (Tree)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Tree as Tree

import PassVeil.Store.Path (Path)
import qualified PassVeil.Console as Console
import qualified PassVeil.Store.Path as Path

data TreeNode
  = PathNode Text
  | KeyNode Text

instance Show TreeNode where
  show (PathNode label) = Text.unpack label
  show (KeyNode label) = Text.unpack label <> " * "

fromPaths :: [Path] -> Tree TreeNode
fromPaths = Tree.unfoldTree build . (PathNode "/",) . map Path.explode
  where
    grp xs ys = NonEmpty.head xs == NonEmpty.head ys

    build (node, rest) = (node, catMaybes $ map level (groupBy grp rest))

    level ns@((node :| rest):_) = Just
      ( bool PathNode KeyNode (null rest) node
      , map NonEmpty.fromList (filter (not . null) (map NonEmpty.tail ns))
      )

    level _ = Nothing

draw :: [Path] -> IO ()
draw = mapM_ (mapM_ printDoc) . draw' . fromPaths

data Doc
  = DocText Text
  | DocNode TreeNode

printDoc :: Doc -> IO ()
printDoc (DocText s) = Text.putStr s
printDoc (DocNode (PathNode path)) =
  Text.putStrLn path
printDoc (DocNode (KeyNode key)) =
  Console.withColor Console.Cyan $
    Text.putStrLn key

draw' :: Tree TreeNode -> [[Doc]]
draw'(Tree.Node x forest) =
  [DocNode x] : drawSubTrees forest
  where
    drawSubTrees :: [Tree TreeNode] -> [[Doc]]
    drawSubTrees [] = []
    drawSubTrees [t] =
      shift [DocText "`-- "] [DocText "  "] (draw' t)
    drawSubTrees (t:ts) =
      shift [DocText "|-- "] [DocText "| "] (draw' t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
