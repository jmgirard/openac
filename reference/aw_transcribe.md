# Transcribe an audio stream using the specified Whisper model

If provided a pre-prepared wav file, will directly transcribe it. But if
provided an unsupported audio format or video file, will extract and
convert the specified audio stream before transcribing it. Set `wavfile`
if you want to retain this conversion or leave it NULL to create and
later discard a temporary wav file.

## Usage

``` r
aw_transcribe(
  infile,
  model,
  language = "auto",
  wavfile = NULL,
  rdsfile = NULL,
  csvfile = NULL,
  audio_args = list(),
  whisper_args = list()
)
```

## Arguments

- infile:

  A required string indicating the file path to an audio or video file
  containing an audio stream to transcribe.

- model:

  A required model object produced by `audio.whisper::whisper()`.

- language:

  The language of the audio. Defaults to 'auto'. For a list of all
  languages the model can handle: see
  `audio.whisper::whisper_languages()`.

- wavfile:

  Either NULL or a string indicating the path to save the prepared
  version of `infile` to (must end with '.wav'). If NULL, a temporary
  file will be created and later discarded.

- rdsfile:

  Either NULL or a string indicating the path to save the full whisper
  output list object to (must end with '.rds').

- csvfile:

  Either NULL or a string indicating the path to save a human-readable
  version of the transcript to (must end with '.csv').

- audio_args:

  A list of optional arguments to forward to
  [`aw_prep_audio`](https://jmgirard.github.io/openac/reference/aw_prep_audio.md).

- whisper_args:

  A list of optional arguments to forward to
  `audio.whisper::predict.whisper()`.

## Value

A list object containing the full whisper output.
