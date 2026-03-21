module LspRecorder.Env (
    getOsInfo,
    extractServerInfo,
) where

import Data.Aeson (decodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Value (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import LspRecorder.Lsp.Types (ServerInfo (..))
import System.Info qualified as SI

-- | Return a string like "linux-x86_64" describing the current platform.
getOsInfo :: IO Text
getOsInfo = pure $ T.pack SI.os <> "-" <> T.pack SI.arch

-- | Try to extract server_info from an LSP initialize response.
-- Looks for: result.serverInfo.{name, version}
extractServerInfo :: ByteString -> Maybe ServerInfo
extractServerInfo bs = do
    val <- decodeStrict bs
    result <- lookupObj "result" val
    siVal <- lookupObj "serverInfo" result
    name <- lookupText "name" siVal
    let version = lookupText "version" siVal
    pure $ ServerInfo{serverInfoName = name, serverInfoVersion = version}
  where
    lookupObj :: Text -> Value -> Maybe Value
    lookupObj k (Object o) = KM.lookup (Key.fromText k) o
    lookupObj _ _ = Nothing

    lookupText :: Text -> Value -> Maybe Text
    lookupText k obj = case lookupObj k obj of
        Just (String t) -> Just t
        _ -> Nothing
