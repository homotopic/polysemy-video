{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- |
--   Module    : Polysemy.Video
--   License   : MIT
--   Stability : experimental
--
-- Experimental Video processing DSL for Polysemy.
module Polysemy.Video
  ( ClipProcess (..),
    extractAudio,
    extractClips,
    extractFrames,
    runFFMpegCli,
    traceFFMpegArgs,
    ignoreClipProcess,
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import Formatting
import Media.Timestamp
import Media.Timestamp.Formatting
import Path
import Path.Formatting
import Polysemy
import Polysemy.Trace
import qualified Turtle as S

-- | Effect for disecting a video file.
--
-- @since 0.1.1.0
data ClipProcess m a where
  ExtractAudio :: Path b File -> [(Range, Path b' File)] -> ClipProcess m ()
  ExtractClips :: Path b File -> [(Range, Path b' File)] -> ClipProcess m ()
  ExtractFrames :: Path b File -> [(Time, Path b' File)] -> ClipProcess m ()

makeSem ''ClipProcess

-- | "-ss <x>" where x is a timestamp.
seekFF :: Time -> [Text]
seekFF t = ["-ss", sformat timef t]

-- | "-ss <x> -to <y> <output>".
rangeFF :: Range -> Path b File -> [Text]
rangeFF (Range f t) x = seekFF f ++ ["-to", sformat timef t, sformat pathf x]

-- | "-ss <x> -vframes 1 <output>"
frameFF :: Time -> Path b File -> [Text]
frameFF t x = seekFF t ++ ["-vframes", "1", sformat pathf x]

-- | "-i <output>"
inputFF :: Path b File -> [Text]
inputFF x = ["-i", sformat pathf x]

-- | "ffmpeg -y" followed by some arguments.
runffmpeg :: MonadIO m => [Text] -> m ()
runffmpeg xs = S.sh $ S.inproc "ffmpeg" ("-y" : "-loglevel" : "warning" : xs) mempty

-- | "mkdir -p" with a `Path b Dir`.
mktreeFP :: MonadIO m => Path b Dir -> m ()
mktreeFP = S.mktree . S.decodeString . toFilePath

-- | Interpret `ClipProcess` by running it against ffmpeg on the command line.
--
-- @since 0.2.0.0
runFFMpegCli :: Member (Embed IO) effs => Sem (ClipProcess ': effs) a -> Sem effs a
runFFMpegCli = interpret $ \case
  ExtractAudio x ts -> mapM_ mktreeFP (parent . snd <$> ts) >> runffmpeg (inputFF x <> (uncurry rangeFF =<< ts))
  ExtractClips x ts -> mapM_ mktreeFP (parent . snd <$> ts) >> runffmpeg (inputFF x <> (uncurry rangeFF =<< ts))
  ExtractFrames x ts -> mapM_ mktreeFP (parent . snd <$> ts) >> runffmpeg (inputFF x <> (uncurry frameFF =<< ts))

-- | Trace `ClipProcess` by printing out the arguments it would pass to ffmpeg.
--
-- @since 0.2.0.0
traceFFMpegArgs :: Members '[ClipProcess, Trace] r => Sem r a -> Sem r a
traceFFMpegArgs = intercept $ \case
  ExtractAudio x ts -> do
    trace $ show $ inputFF x <> (uncurry rangeFF =<< ts)
    extractAudio x ts
  ExtractClips x ts -> do
    trace $ show $ inputFF x <> (uncurry rangeFF =<< ts)
    extractClips x ts
  ExtractFrames x ts -> do
    trace $ show $ inputFF x <> (uncurry frameFF =<< ts)
    extractFrames x ts

-- | Noop the `ClipProcess` effect.
--
-- @since 0.2.0.0
ignoreClipProcess :: Sem (ClipProcess ': r) a -> Sem r a
ignoreClipProcess = interpret $ \case
  ExtractAudio _ _ -> pure ()
  ExtractClips _ _ -> pure ()
  ExtractFrames _ _ -> pure ()
