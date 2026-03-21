-- | Project snapshot: collect matching files and archive them with tar+zstd.
module LspRecorder.Snapshot
  ( collectFiles
  , takeSnapshot
  , restoreSnapshot
  ) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.Zstd qualified as Zstd
import Data.ByteString.Lazy qualified as BL
import LspRecorder.Config (SnapshotConfig (..))
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>))
import System.FilePattern ((?==))

-- | Recursively walk @root@ and return relative paths of files that match at
-- least one include glob and none of the exclude globs. Directories matching
-- an exclude glob are pruned without being descended into.
collectFiles :: FilePath -> SnapshotConfig -> IO [FilePath]
collectFiles root SnapshotConfig{scInclude, scExclude} = go ""
  where
    go relDir = do
      let absDir = if null relDir then root else root </> relDir
      entries <- listDirectory absDir
      fmap concat $ mapM (processEntry relDir) entries

    processEntry relDir name = do
      let relPath = if null relDir then name else relDir </> name
          absPath = root </> relPath
      isDir <- doesDirectoryExist absPath
      isFile <- doesFileExist absPath
      if isDir
        then
          if matchesAny scExclude relPath
            then pure []
            else go relPath
        else
          if isFile && matchesAny scInclude relPath && not (matchesAny scExclude relPath)
            then pure [relPath]
            else pure []

    matchesAny pats path = any (?== path) pats

-- | Collect matching files from @projectRoot@, bundle them into a tar archive,
-- compress with zstd, and write to @archivePath@.
takeSnapshot :: SnapshotConfig -> FilePath -> FilePath -> IO ()
takeSnapshot cfg projectRoot archivePath = do
  relPaths <- collectFiles projectRoot cfg
  entries <- Tar.pack projectRoot relPaths
  let tarBytes = Tar.write entries
      compressed = Zstd.compress 3 (BL.toStrict tarBytes)
  BL.writeFile archivePath (BL.fromStrict compressed)

-- | Decompress and unpack a snapshot archive (produced by 'takeSnapshot') into
-- @targetDir@. The directory must already exist.
restoreSnapshot :: FilePath -> FilePath -> IO ()
restoreSnapshot archivePath targetDir = do
  compressed <- BL.readFile archivePath
  tarBytes <- case Zstd.decompress (BL.toStrict compressed) of
    Zstd.Skip -> fail "restoreSnapshot: zstd decompression returned Skip"
    Zstd.Error msg -> fail $ "restoreSnapshot: zstd decompression error: " <> msg
    Zstd.Decompress bs -> pure (BL.fromStrict bs)
  Tar.unpack targetDir (Tar.read tarBytes)
