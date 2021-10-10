module PassVeil.Console
  ( module System.Console.ANSI
  , withColor
  , spacer
  )
where

import qualified Data.List as List

import System.Console.ANSI
import qualified System.Console.ANSI as Console

withColor :: Console.Color -> IO () -> IO ()
withColor color act = do
  Console.setSGR c
  act
  Console.setSGR [Console.Reset]
  where
    c = [ Console.SetColor
            Console.Foreground
            Console.Dull
            color
        ]

spacer :: [IO ()] -> IO ()
spacer acts = sequence_ $
  List.intercalate [putChar '\n'] (map (:[]) acts)
