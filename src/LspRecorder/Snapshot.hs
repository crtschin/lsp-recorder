-- | Project snapshot: collect matching files and archive them with tar+zstd.
module LspRecorder.Snapshot
  ( collectFiles
  , takeSnapshot
  , restoreSnapshot
  , snapshotArchivePath
  ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.Zstd qualified as Zstd
import Data.ByteString.Lazy qualified as BL
import LspRecorder.Config (SnapshotConfig (..))
import System.Directory.OsPath
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePattern ((?==))
import System.OsPath (OsPath, decodeFS, encodeFS, takeBaseName, takeDirectory, (</>))

-- | Recursively walk @root@ and return relative paths of files that match at
-- least one include glob and none of the exclude globs. Directories matching
-- an exclude glob are pruned without being descended into.
collectFiles :: OsPath -> SnapshotConfig -> IO [OsPath]
collectFiles root SnapshotConfig{scInclude, scExclude} = go Nothing
 where
  go mRelDir = do
    let absDir = maybe root (root </>) mRelDir
    entries <- listDirectory absDir
    fmap concat $ mapM (processEntry mRelDir) entries

  processEntry mRelDir name = do
    let relPath = maybe name (</> name) mRelDir
        absPath = root </> relPath
    relPathStr <- decodeFS relPath
    isDir <- doesDirectoryExist absPath
    isFile <- doesFileExist absPath
    if isDir
      then
        if matchesAny scExclude relPathStr
          then pure []
          else go (Just relPath)
      else
        if isFile && matchesAny scInclude relPathStr && not (matchesAny scExclude relPathStr)
          then pure [relPath]
          else pure []

  matchesAny pats path = any (?== path) pats

-- | Collect matching files from @projectRoot@, bundle them into a tar archive,
-- compress with zstd, and write to @archivePath@.
takeSnapshot :: SnapshotConfig -> OsPath -> OsPath -> IO ()
takeSnapshot cfg projectRoot archivePath = do
  relPaths <- collectFiles projectRoot cfg
  projectRootStr <- decodeFS projectRoot
  relPathStrs <- mapM decodeFS relPaths
  archivePathStr <- decodeFS archivePath
  entries <- Tar.pack projectRootStr relPathStrs
  let tarBytes = Tar.write entries
      compressed = Zstd.compress 3 (BL.toStrict tarBytes)
  BL.writeFile archivePathStr (BL.fromStrict compressed)

-- | Derive the snapshot archive path from a trace file path.
-- e.g. @"session.jsonl"@ → @"session.snapshot.tar.zst"@ in the same directory.
snapshotArchivePath :: OsPath -> IO OsPath
snapshotArchivePath tracePath = do
  suffix <- encodeFS ".snapshot.tar.zst"
  pure $ takeDirectory tracePath </> (takeBaseName tracePath <> suffix)

-- | Decompress and unpack a snapshot archive (produced by 'takeSnapshot') into
-- @targetDir@. The directory must already exist.
restoreSnapshot :: OsPath -> OsPath -> IO ()
restoreSnapshot archivePath targetDir = do
  archivePathStr <- decodeFS archivePath
  targetDirStr <- decodeFS targetDir
  compressed <- BL.readFile archivePathStr
  tarBytes <- case Zstd.decompress (BL.toStrict compressed) of
    Zstd.Skip -> fail "restoreSnapshot: zstd decompression returned Skip"
    Zstd.Error msg -> fail $ "restoreSnapshot: zstd decompression error: " <> msg
    Zstd.Decompress bs -> pure (BL.fromStrict bs)
  Tar.unpack targetDirStr (Tar.read tarBytes)
