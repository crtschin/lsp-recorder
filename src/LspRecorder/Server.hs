module LspRecorder.Server
  ( spawnServer
  , cleanupServer
  , withServer
  ) where

import Control.Exception (SomeException, bracket, catch)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hPutStrLn, stderr)
import System.OsPath (OsPath, decodeFS)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , createProcess
  , shell
  , terminateProcess
  , waitForProcess
  )

spawnServer :: Maybe OsPath -> String -> IO (Handle, Handle, ProcessHandle)
spawnServer mcwd cmd = do
  hPutStrLn stderr $ "[lsp-recorder] spawning server: " <> cmd
  mcwdStr <- traverse decodeFS mcwd
  let cp = (shell cmd){std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit, cwd = mcwdStr}
  (Just sIn, Just sOut, Nothing, ph) <- createProcess cp
  pure (sIn, sOut, ph)

cleanupServer :: (Handle, Handle, ProcessHandle) -> IO ()
cleanupServer (sIn, sOut, ph) = do
  hPutStrLn stderr "[lsp-recorder] terminating server"
  terminateProcess ph `catch` \(e :: SomeException) -> hPutStrLn stderr $ "[lsp-recorder] terminateProcess error: " <> show e
  _ <-
    waitForProcess ph `catch` \(e :: SomeException) -> do
      hPutStrLn stderr $ "[lsp-recorder] waitForProcess error: " <> show e
      pure ExitSuccess
  hClose sIn `catch` \(_ :: SomeException) -> pure ()
  hClose sOut `catch` \(_ :: SomeException) -> pure ()

withServer :: Maybe OsPath -> String -> ((Handle, Handle, ProcessHandle) -> IO a) -> IO a
withServer mcwd cmd = bracket (spawnServer mcwd cmd) cleanupServer
