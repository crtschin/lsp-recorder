module LspRecorder.Server
  ( spawnServer
  , cleanupServer
  , withServer
  ) where

import Control.Exception (SomeException, bracket, catch)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , createProcess
  , shell
  , terminateProcess
  , waitForProcess
  )

spawnServer :: String -> IO (Handle, Handle, ProcessHandle)
spawnServer cmd = do
  let cp = (shell cmd){std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
  (Just sIn, Just sOut, Nothing, ph) <- createProcess cp
  pure (sIn, sOut, ph)

cleanupServer :: (Handle, Handle, ProcessHandle) -> IO ()
cleanupServer (sIn, sOut, ph) = do
  terminateProcess ph `catch` \(_ :: SomeException) -> pure ()
  _ <- waitForProcess ph `catch` \(_ :: SomeException) -> pure ExitSuccess
  hClose sIn `catch` \(_ :: SomeException) -> pure ()
  hClose sOut `catch` \(_ :: SomeException) -> pure ()

withServer :: String -> ((Handle, Handle, ProcessHandle) -> IO a) -> IO a
withServer cmd = bracket (spawnServer cmd) cleanupServer
