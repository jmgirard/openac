# Run aw_prep_audio() on multiple files in a directory

Find all media files with a specified extension in a specified directory
and then extract an audio file for acoustic analysis from each.

## Usage

``` r
aw_prep_audio_dir(
  indir,
  inext,
  outdir,
  recursive = FALSE,
  progress = c("auto", "on", "off"),
  ...
)
```

## Arguments

- indir:

  (string) What directory contains the input files?

- inext:

  (string) What file extension should be looked for in `indir` (e.g.,
  "mp4" or "mp3")? Matched regardless of case, so "mp4" also takes
  `.MP4` files; if that leaves two inputs deriving the same output file,
  the batch is refused rather than one silently overwriting the other.

- outdir:

  (string) What directory should the audio files be output to?

- recursive:

  (logical, default = FALSE) Should files in subdirectories within
  `indir` be included?

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
  [`aw_prep_audio`](https://jmgirard.github.io/openac/reference/aw_prep_audio.md)

  `stream`

  :   An optional number indicating the index of the audio stream in
      `infile` to convert or extract. Note that ffmpeg uses
      zero-indexing so the default of 0 is the first stream. Defaults to
      0.

  `overwrite`

  :   Should outfile be overwritten if it already exists? It will be
      skipped otherwise, silently for a direct call. In a batch the row
      depends on whose job the preparing is: under `aw_prep_audio_dir()`
      it is the whole job, so the row reads `"skipped"`; under
      [`aw_transcribe_dir()`](https://jmgirard.github.io/openac/reference/aw_transcribe_dir.md)
      the existing file is reused and whisper still runs, so the row
      reads `"ok"`. Defaults to TRUE.

  `afilters`

  :   Should audio filters be used to try to improve audio quality? (See
      Details.) Defaults to FALSE.

## Value

(Invisibly) a data frame with one row per input file, giving the
`infile` and `outfile` it was called with, its `status`, whether it
`success`ed, and the `error` message if it did not. `status` is one of
`"ok"` (the operation completed), `"skipped"` (the file was deliberately
not processed) or `"failed"` (the operation raised an error); `success`
is `status == "ok"`, so a skipped file reads `FALSE`, and `error`
carries the reason for a skipped file as well as for a failed one. A
file that fails does not abort the batch: it is warned about, recorded
as `"failed"`, and the remaining files still run.

## Details

Can be optionally run in parallel by running
[`plan()`](https://future.futureverse.org/reference/plan.html)
beforehand, e.g., by calling `plan("multisession", workers = 4)`.
