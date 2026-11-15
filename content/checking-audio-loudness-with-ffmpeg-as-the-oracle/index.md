+++
title = "Checking audio loudness from Haskell, with FFmpeg as the oracle"
date = 2026-11-15
[taxonomies]
authors = ["Mark Santos"]
categories = ["Community"]
tags = ["Testing", "QuickCheck", "Practices"]
+++

Audio for streaming platforms is delivered at a loudness target, usually around -14 LUFS, with true peaks below -1 dBTP. FFmpeg's `loudnorm` filter measures both. This post wraps that measurement in a small Haskell library, then tests the library with QuickCheck properties that run FFmpeg on generated signals whose loudness is known in advance.

<!-- more -->

I edit video for a living, and checking loudness before delivery is a daily job. The interesting part here is the testing: a property test caught my own wrong assumption about FFmpeg in its first run. Everything below runs with GHC 9.14.1, cabal 3.16 and FFmpeg 5.1.

## What FFmpeg prints

The first pass of `loudnorm` analyses the file and prints a JSON block to stderr, after FFmpeg's usual log lines:

```
[Parsed_loudnorm_0 @ 0xaaaad6a5c9c0]
{
	"input_i" : "-14.03",
	"input_tp" : "-1.20",
	"input_lra" : "0.00",
	...
}
```

Two details shape the parser. Every number is a string, and when there is nothing to measure (silence, or a clip shorter than one 400 ms measurement block) the value is `"-inf"`. That gives a natural type:

```haskell
-- | Integrated loudness (LUFS) and true peak (dBTP). 'Nothing' means FFmpeg
-- reported -inf: silence, or a clip too short to measure.
data Loudness = Loudness
  { integrated :: Maybe Double
  , truePeak :: Maybe Double
  }
  deriving (Show, Eq)

instance FromJSON Loudness where
  parseJSON = withObject "loudnorm" $ \o ->
    Loudness <$> (o .: "input_i" >>= decibels) <*> (o .: "input_tp" >>= decibels)

decibels :: T.Text -> Parser (Maybe Double)
decibels "-inf" = pure Nothing
decibels t = maybe (fail ("not a number: " <> T.unpack t)) (pure . Just) (readMaybe (T.unpack t))
```

The JSON block is the last `{...}` on stderr, so the parser slices it out before decoding:

```haskell
parseLoudnorm :: BL.ByteString -> Either String Loudness
parseLoudnorm err =
  case (BL.elemIndexEnd '{' err, BL.elemIndexEnd '}' err) of
    (Just a, Just b) | a < b -> eitherDecode (BL.take (b - a + 1) (BL.drop a err))
    _ -> Left "no loudnorm JSON in ffmpeg's output"
```

## Running FFmpeg

[typed-process](https://hackage.haskell.org/package/typed-process) reads stdout and stderr together without the classic deadlock of draining one pipe while the other fills, and gives back the exit code:

```haskell
measure :: FilePath -> IO (Either String Loudness)
measure path = do
  (code, _out, err) <-
    readProcess $
      proc "ffmpeg" ["-hide_banner", "-nostats", "-i", path, "-af", "loudnorm=print_format=json", "-f", "null", "-"]
  pure $ case code of
    ExitFailure n -> Left ("ffmpeg exited with " <> show n <> ": " <> lastLine err)
    ExitSuccess -> parseLoudnorm err
```

A missing or corrupt file comes back as a `Left` carrying FFmpeg's last error line, not as an exception.

## The check

A delivery spec is a target, a tolerance and a true-peak ceiling, and `check` returns every way a file misses it, so an empty list means it passes:

```haskell
data Problem = TooQuiet Double | TooLoud Double | PeakTooHigh Double | NotMeasurable
  deriving (Show, Eq)

check :: Spec -> Loudness -> [Problem]
check spec (Loudness i tp) = loudness ++ peak
  where
    loudness = case i of
      Nothing -> [NotMeasurable]
      Just lufs
        | lufs < target spec - tolerance spec -> [TooQuiet lufs]
        | lufs > target spec + tolerance spec -> [TooLoud lufs]
        | otherwise -> []
    peak = case tp of
      Just dbtp | dbtp > maxTruePeak spec -> [PeakTooHigh dbtp]
      _ -> []
```

## Testing with signals of known loudness

Unit tests for the parser are easy. Testing `measure` itself needs inputs whose answer is known, and the loudness standard provides one. [ITU-R BS.1770](https://www.itu.int/rec/R-REC-BS.1770) is calibrated so that a 997 Hz sine in one channel reads 3.01 LU below its peak level. A full-scale tone reads -3.01 LUFS, a tone peaking at -20 dBFS reads -23.01, and so on for any gain.

That is a property, and FFmpeg can generate the tones. One detail: FFmpeg's `sine` source starts at 1/8 of full scale, about -18.06 dBFS, so the generator adds that back to get an exact peak:

```haskell
tone :: FilePath -> Double -> Double -> Channels -> IO FilePath
tone dir peakDb seconds channels = do
  let out = dir </> ("tone" <> show peakDb <> show channels <> ".wav")
      gain = peakDb + 20 * logBase 10 8
  ...
```

The property then picks random peak levels, renders a tone for each into a temporary directory and checks both numbers:

```haskell
testProperty "a 997 Hz tone reads 3.01 LU below its peak" $
  withNumTests 12 $ forAll (choose (-40, -1)) $ \peak -> ioProperty $
    withSystemTempDirectory "gate" $ \dir -> do
      Loudness (Just i) (Just tp) <- measured =<< tone dir peak 4 Mono
      pure $ counterexample (show (peak, i, tp)) $
        near 0.1 tp peak .&&. near 0.1 i (peak - 3.01)
```

`ioProperty` lets each case run real IO, and `withSystemTempDirectory` cleans up after it. Each case takes about 0.15 seconds, so twelve random levels cost under two seconds.

## The property that was wrong

BS.1770 adds up the power of all channels, so the same signal in two channels should read 3.01 LU louder than in one. My first version of that property built the stereo file with FFmpeg's `-ac 2` and failed on its first case:

```
a second identical channel adds 3.01 LU: FAIL
  *** Failed! Falsified (after 1 test):
  -26.44386513912398
  (-29.45,-29.45)
```

Mono and "stereo" read exactly the same. The property was right and my assumption was wrong. When `-ac 2` upmixes a mono source, it spreads it over two channels at -3 dB each, so the total power, and therefore the loudness, doesn't change. Checked by hand on FFmpeg 9.0.2, a tone's peak sample dropped from 4095 to 2896 under `-ac 2`, while its integrated loudness stayed at -21.11 LUFS. Copying the channel with `pan=stereo|c0=c0|c1=c0` instead gives two identical channels, and the property passes. Both behaviours are now tested:

```
a second identical channel adds 3.01 LU:   OK
  +++ OK, passed 6 tests.
-ac 2 keeps the loudness of a mono source: OK
  +++ OK, passed 6 tests.
```

This matters outside tests too. A pipeline that "makes everything stereo" with `-ac 2` before normalizing behaves differently from one that copies channels, and the difference is exactly 3 dB.

## Wrapping up

The whole library is about sixty lines of code: a parser that respects FFmpeg's string numbers and `-inf`, `measure` on top of typed-process, and a pure `check`. The test suite has three parser tests, four plain integration tests (an unmeasurable 0.2-second clip, a passing tone, a tone that is too loud and too hot, a missing file) and three properties that drive FFmpeg with signals of known loudness. All ten pass in about six seconds.

Testing a wrapper by feeding the tool inputs with a known answer worked better than mocking it would have. A mock would have encoded my belief about `-ac 2`, not FFmpeg's behaviour.
