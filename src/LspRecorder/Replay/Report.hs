module LspRecorder.Replay.Report
  ( MethodStats (..)
  , ReplayReport (..)
  , generateReport
  , writeReport
  ) where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.IO (IOMode (..), withFile)

data MethodStats = MethodStats
  { msCount :: Int
  , msP50Ms :: Double
  , msP95Ms :: Double
  , msP99Ms :: Double
  }
  deriving stock (Eq, Show)

instance ToJSON MethodStats where
  toJSON MethodStats{msCount, msP50Ms, msP95Ms, msP99Ms} =
    object
      [ "count" .= msCount
      , "p50_ms" .= msP50Ms
      , "p95_ms" .= msP95Ms
      , "p99_ms" .= msP99Ms
      ]

data ReplayReport = ReplayReport
  { rrTrace :: FilePath
  , rrTimingMode :: Text
  , rrTotalDurationMs :: Int
  , rrTimedOut :: Int
  , rrMethods :: Map Text MethodStats
  }
  deriving stock (Eq, Show)

instance ToJSON ReplayReport where
  toJSON ReplayReport{rrTrace, rrTimingMode, rrTotalDurationMs, rrTimedOut, rrMethods} =
    object
      [ "trace" .= rrTrace
      , "timing_mode" .= rrTimingMode
      , "total_duration_ms" .= rrTotalDurationMs
      , "timed_out_requests" .= rrTimedOut
      , "methods" .= rrMethods
      ]

-- | Build per-method stats from a list of (method, latency_ms) pairs.
generateReport :: [(Text, Double)] -> Map Text MethodStats
generateReport pairs =
  Map.map computeStats grouped
 where
  grouped :: Map Text [Double]
  grouped = foldr (\(m, l) acc -> Map.insertWith (++) m [l] acc) Map.empty pairs

  computeStats :: [Double] -> MethodStats
  computeStats latencies =
    let sorted = sort latencies
        n = length sorted
     in MethodStats
          { msCount = n
          , msP50Ms = percentile sorted n 50
          , msP95Ms = percentile sorted n 95
          , msP99Ms = percentile sorted n 99
          }

  percentile :: [Double] -> Int -> Int -> Double
  percentile sorted n pct
    | n == 0 = 0
    | otherwise =
        let idx = max 0 (min (n - 1) (ceiling (fromIntegral n * fromIntegral pct / 100.0 :: Double) - 1))
         in sorted !! idx

writeReport :: FilePath -> ReplayReport -> IO ()
writeReport path report =
  withFile path WriteMode $ \h ->
    BL.hPutStr h (encode report)
