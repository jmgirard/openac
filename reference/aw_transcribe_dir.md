# Transcribe multiple media files with Whisper

Find all files in a specified directory with a specified extension and
then apply
[`aw_transcribe`](https://jmgirard.github.io/openac/reference/aw_transcribe.md)
to each to transcribe them. If the input files are not in the format
expected by Whisper, they will be converted first.

## Usage

``` r
aw_transcribe_dir(
  indir,
  inext,
  wavdir = NULL,
  rdsdir = NULL,
  csvdir = NULL,
  recursive = FALSE,
  progress = c("auto", "on", "off"),
  ...
)
```

## Arguments

- indir:

  (character) What directory contains the input files?

- inext:

  (character) What file extension should be looked for in `indir` (e.g.,
  "mp4" or "mp3")? Matched regardless of case, so "mp4" also takes
  `.MP4` files; if that leaves two inputs deriving the same output file,
  the batch is refused rather than one silently overwriting the other.

- wavdir:

  (character, default=NULL) What directory should the prepared WAV files
  be saved to? If `NULL`, temporary WAV files will be created and later
  discarded.

- rdsdir:

  (character, default=NULL) What directory should the RDS output files
  be saved to? If `NULL`, RDS files will not be output.

- csvdir:

  (character, default=NULL) What directory should the CSV output files
  be saved to? If `NULL`, CSV files will not be output.

- recursive:

  (logical, default=FALSE) Should files in subdirectories within `indir`
  be included?

- progress:

  (string, default = "auto") Controls progress reporting. One of
  `"auto"`, `"on"`, or `"off"`.

  - `"auto"`: Emit `progressr` signals but do not force display; a
    progress bar appears only if the caller has enabled a handler (e.g.,
    [`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html)
    or a global handler).

  - `"on"`: Wraps the call in
    [`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html)
    so progress will render using any available handler.

  - `"off"`: Suppresses progress signals via
    [`progressr::without_progress()`](https://progressr.futureverse.org/reference/with_progress.html).

- ...:

  Arguments passed on to
  [`aw_transcribe`](https://jmgirard.github.io/openac/reference/aw_transcribe.md)

  `model`

  : A required model object produced by `audio.whisper::whisper()`.

  `language`

  : The language of the audio. Defaults to 'auto'. For a list of all
    languages the model can handle: see
    `audio.whisper::whisper_languages()`.

  `audio_args`

  : A list of optional arguments to forward to
    [`aw_prep_audio`](https://jmgirard.github.io/openac/reference/aw_prep_audio.md).

  `whisper_args`

  : A list of optional arguments to forward to
    `audio.whisper::predict.whisper()`.

## Value

(Invisibly) a data frame with one row per input file, giving the paths
it was called with, its `status`, whether it `success`ed, and the
`error` message if it did not. `status` is one of `"ok"` (the operation
completed), `"skipped"` (the file was deliberately not processed) or
`"failed"` (the operation raised an error); `success` is
`status == "ok"`, so a skipped file reads `FALSE`, and `error` carries
the reason for a skipped file as well as for a failed one. A file that
fails does not abort the batch: it is warned about, recorded as
`"failed"`, and the remaining files still run.

## Details

Can optionally output a progress bar by using
[`handlers`](https://progressr.futureverse.org/reference/handlers.html),
e.g., by calling `handlers("cli"); handlers(global = TRUE)` before this
code.

Cannot be run in parallel due to using the GPU.
