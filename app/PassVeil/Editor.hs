module PassVeil.Editor (edit) where

import Control.Monad (forM_, when)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import PassVeil.Exit (ExitCode(ExitSuccess))
import qualified PassVeil.Exit as Exit

import System.IO (hClose)
import qualified System.Environment as Environment
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

edit :: Maybe Text -> (Text -> IO ()) -> IO ()
edit mContent act = do
  editor <- Environment.getEnv "EDITOR"

  when (null editor) $
    Exit.couldNotRunEditor

  Temp.withSystemTempFile "passveil" $ \f h -> do
    forM_ mContent (Text.hPutStr h)

    hClose h

    exitCode <- Process.system $
      editor ++ " " ++ f

    when (exitCode /= ExitSuccess) $
      Exit.couldNotEdit

    secret <- Text.dropWhileEnd (== '\n')  <$> Text.readFile f

    when (Text.null secret) $
       Exit.nothingToDo

    act secret
