{- |
   Module    : Polysemy.Video
   License   : MIT
   Stability : experimental

Experimental Video processing DSL for Polysemy.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Polysemy.Video where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Formatting
import           Path
import           Path.Utils
import           Polysemy
import qualified Turtle                 as S

-- | Timestamp data type.
data Time = Time
  { hour    :: Int
  , minutes :: Int
  , seconds :: Int
  , frame   :: Int
  } deriving (Eq, Ord, Show)

-- | Interval of two timestamps.
data Range = Range
  { from :: Time
  , to   :: Time
  } deriving (Eq, Ord, Show)

-- |
data ClipProcess m a where
  ExtractAudio  :: Path b File -> [(Range, Path b' File)] -> ClipProcess m ()
  ExtractClips  :: Path b File -> [(Range, Path b' File)] -> ClipProcess m ()
  ExtractFrames :: Path b File -> [(Time, Path b' File)]  -> ClipProcess m ()

makeSem ''ClipProcess

-- | Format a Timestamp to ffmpeg's format 00:00:00.000.
timeFF :: Time -> Text
timeFF (Time h m s f) = sformat (int % ":" % int % ":" % int % "." % int) h m s f

-- | "-ss <x>" where x is a timestamp.
seekFF :: Time -> [Text]
seekFF t = ["-ss", timeFF t]

-- | "-ss <x> -to <y> <output>".
rangeFF :: Range -> Path b File -> [Text]
rangeFF (Range f t) x = seekFF f ++ ["-to", timeFF t, toFilePathText x]

-- | "-ss <x> -vframes 1 <output>"
frameFF :: Time -> Path b File -> [Text]
frameFF t x = seekFF t ++ ["-vframes", "1", toFilePathText x]

-- | "-i <output>"
inputFF :: Path b File -> [Text]
inputFF x = ["-i", toFilePathText x]

-- | "ffmpeg -y" followed by some arguments.
runffmpeg :: MonadIO m => [Text] -> m ()
runffmpeg xs = S.sh $ S.inproc "ffmpeg" ("-y" : "-loglevel" : "warning" : xs) mempty

-- | "mkdir -p" with a `Path b Dir`.
mktreeFP :: MonadIO m => Path b Dir -> m ()
mktreeFP = S.mktree . S.decodeString . toFilePath

-- | Interpret `ClipProcess` by running it against ffmpeg on the command line.
interpretFFMpegCli :: Member (Embed IO) effs => Sem (ClipProcess ': effs) a -> Sem effs a
interpretFFMpegCli = interpret $ \case
  ExtractAudio  x ts -> mapM_ mktreeFP (parent . snd <$> ts) >> runffmpeg (inputFF x <> (uncurry rangeFF =<< ts))
  ExtractClips  x ts -> mapM_ mktreeFP (parent . snd <$> ts) >> runffmpeg (inputFF x <> (uncurry rangeFF =<< ts))
  ExtractFrames x ts -> mapM_ mktreeFP (parent . snd <$> ts) >> runffmpeg (inputFF x <> (uncurry frameFF =<< ts))

-- | Interpret `ClipProcess` by printing out the command it would have run to the terminal.
interpretFFMpegNoop :: Member (Embed IO) effs => Sem (ClipProcess ': effs) a -> Sem effs a
interpretFFMpegNoop = interpret $ \case
  ExtractAudio x ts  -> embed $ print $ T.unwords $ inputFF x <> (uncurry rangeFF =<< ts)
  ExtractClips x ts  -> embed $ print $ T.unwords $ inputFF x <> (uncurry rangeFF =<< ts)
  ExtractFrames x ts -> embed $ print $ T.unwords $ inputFF x <> (uncurry frameFF =<< ts)
