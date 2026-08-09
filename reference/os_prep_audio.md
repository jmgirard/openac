# Prepare an audio stream for analysis by opensmile

Import an audio or video file and export an audio file for acoustic
analysis. Extract the audio stream specified by `stream` and then
transcode it to a mono (i.e., single channel) 16-bit PCM .wav file at
44.1kHz sampling rate.

## Usage

``` r
os_prep_audio(infile, outfile, stream = 0, overwrite = TRUE)
```

## Arguments

- infile:

  (character) What is the filepath of the audio or video file to import?

- outfile:

  (character) What is the filepath of the .wav file to create?

- stream:

  (numeric, default=0) The index of the audio stream to extract (ffmpeg
  uses zero-indexing so 0 is the first stream).

- overwrite:

  Should outfile be overwritten if it already exists? It will be skipped
  otherwise, silently for a direct call. In a batch the row depends on
  whose job the preparing is: under
  [`os_prep_audio_dir()`](https://jmgirard.github.io/openac/reference/os_prep_audio_dir.md)
  it is the whole job, so the row reads `"skipped"`; under
  [`os_extract_dir()`](https://jmgirard.github.io/openac/reference/os_extract_dir.md)
  the existing file is reused and openSMILE still runs, so the row reads
  `"ok"`. Defaults to TRUE.

## Value

A character vector containing the output of ffmpeg. Errors, naming the
file, if ffmpeg exits non-zero.
