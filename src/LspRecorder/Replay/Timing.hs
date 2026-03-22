module LspRecorder.Replay.Timing
  ( TimingStrategy (..)
  , realisticStrategy
  , immediateStrategy
  ) where

data TimingStrategy = TimingStrategy
  { tsComputeDelay :: Int -> Int -> Int
  -- ^ prevTimestampUs -> currTimestampUs -> microseconds to wait
  , tsWaitForResponse :: Bool
  -- ^ When 'True', wait for each request's response before sending the next.
  }

-- | Replay with realistic inter-message delays.
realisticStrategy :: TimingStrategy
realisticStrategy =
  TimingStrategy
    { tsComputeDelay = \prev curr ->
        let delta = curr - prev
         in if delta > 0 then delta else 0
    , tsWaitForResponse = False
    }

-- | Replay all messages immediately with no delay.
immediateStrategy :: TimingStrategy
immediateStrategy =
  TimingStrategy
    { tsComputeDelay = \_ _ -> 0
    , tsWaitForResponse = True
    }
