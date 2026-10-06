# Prepare an audio stream for analysis by Whisper

If provided an audio file, convert the specified audio stream to the
proper format for Whisper. Or, if provided a video file, extract the
specified audio stream and convert it to the proper format for Whisper.

## Usage

``` r
aw_prep_audio(infile, outfile, stream = 0, overwrite = TRUE, afilters = FALSE)
```

## Arguments

- infile:

  A required string indicating the filepath to the input audio or video
  file containing the stream to convert or extract.

- outfile:

  A required string indicating the filepath to the audio (.wav) file to
  create containing only the specified audio stream from `infile`.

- stream:

  An optional number indicating the index of the audio stream in
  `infile` to convert or extract. Note that ffmpeg uses zero-indexing so
  the default of 0 is the first stream. Defaults to 0.

- overwrite:

  Should outfile be overwritten if it already exists? It will be skipped
  otherwise, silently for a direct call. In a batch the row depends on
  whose job the preparing is: under
  [`aw_prep_audio_dir()`](https://jmgirard.github.io/openac/reference/aw_prep_audio_dir.md)
  it is the whole job, so the row reads `"skipped"`; under
  [`aw_transcribe_dir()`](https://jmgirard.github.io/openac/reference/aw_transcribe_dir.md)
  the existing file is reused and whisper still runs, so the row reads
  `"ok"`. Defaults to TRUE.

- afilters:

  Should audio filters be used to try to improve audio quality? (See
  Details.) Defaults to FALSE.

## Value

A string containing the text output from ffmpeg. Errors, naming the
file, if ffmpeg exits non-zero.

## Details

The audio filters applied when `afilters = TRUE` are normalizing
loudness (loudnorm), filtering to human speech frequencies (lowpass,
highpass), reducing noise in the frequency domain (afftdn), compressing
dynamic range (compand), dynamically normalizing volume (dynaudnorm),
and boosting subtle transient details (areverse, asubboost, areverse).
