module LspRecorder.Replay.Timing
  ( TimingStrategy (..)
  , realisticStrategy
  , immediateStrategy
  ) where

data TimingStrategy = TimingStrategy
  { tsComputeDelay :: Int -> Int -> Int
  -- ^ prevTimestampUs -> currTimestampUs -> microseconds to wait
  }

-- | Replay with realistic inter-message delays.
realisticStrategy :: TimingStrategy
realisticStrategy = TimingStrategy $ \prev curr ->
  let delta = curr - prev
   in if delta > 0 then delta else 0

-- | Replay all messages immediately with no delay.
immediateStrategy :: TimingStrategy
immediateStrategy = TimingStrategy $ \_ _ -> 0
