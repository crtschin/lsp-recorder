module LspRecorder.Server
  ( spawnServer
  , cleanupServer
  , withServer
  ) where

import Control.Exception (SomeException, bracket, catch)
import Data.List (uncons)
import System.IO (Handle, hClose, hPutStrLn, stderr)
import System.OsPath (OsPath, decodeFS)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , cleanupProcess
  , createProcess
  , proc
  , waitForProcess
  )
import System.Timeout (timeout)

spawnServer :: Maybe OsPath -> String -> IO (Handle, Handle, ProcessHandle)
spawnServer mcwd cmd = do
  hPutStrLn stderr $ "[lsp-recorder] spawning server: " <> cmd
  mcwdStr <- traverse decodeFS mcwd
  let (command, args) = case uncons (words cmd) of
        Just r -> r
        Nothing -> error "expected a non-empty server argument"
  let cp = (proc command args){std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit, cwd = mcwdStr}
  (Just sIn, Just sOut, Nothing, ph) <- createProcess cp
  pure (sIn, sOut, ph)

cleanupServer :: (Handle, Handle, ProcessHandle) -> IO ()
cleanupServer (sIn, sOut, ph) = do
  hPutStrLn stderr "[lsp-recorder] terminating server"
  -- Wait up to 15s for the server to exit on its own (e.g. after receiving
  -- the LSP 'exit' notification), which lets GHC's RTS print +RTS -s stats.
  exited <- timeout 15_000_000 (waitForProcess ph)
  case exited of
    Just _ -> hPutStrLn stderr "[lsp-recorder] server exited cleanly"
    Nothing -> do
      hPutStrLn stderr "[lsp-recorder] server did not exit in time, force-killing"
      cleanupProcess (Just sIn, Just sOut, Nothing, ph) `catch` \(e :: SomeException) ->
        hPutStrLn stderr $ "[lsp-recorder] cleanupProcess error: " <> show e
  -- Always close pipe handles so the RTS can shut down cleanly and print
  -- +RTS -s stats. Ignoring errors since HLS may have already closed them.
  hClose sIn `catch` \(_ :: SomeException) -> pure ()
  hClose sOut `catch` \(_ :: SomeException) -> pure ()

withServer :: Maybe OsPath -> String -> ((Handle, Handle, ProcessHandle) -> IO a) -> IO a
withServer mcwd cmd = bracket (spawnServer mcwd cmd) cleanupServer
