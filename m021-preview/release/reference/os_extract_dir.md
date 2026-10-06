# Run os_extract() on multiple files in a directory

Find all .wav files in a specified directory and then extract opensmile
features from each (according to `config`).

## Usage

``` r
os_extract_dir(
  indir,
  inext,
  wavdir = NULL,
  aggdir = NULL,
  llddir = NULL,
  recursive = FALSE,
  ...
)
```

## Arguments

- indir:

  (character) What directory contains the input .wav files?

- inext:

  (character) What file extension to look for in `indir`? Matched
  regardless of case, so "mp4" also takes `.MP4` files; if that leaves
  two inputs deriving the same output file, the batch is refused rather
  than one silently overwriting the other.

- wavdir:

  (character, default=NULL) What directory should the prepared WAV audio
  files be saved to? If `NULL`, temporary WAV files will be created and
  then discarded (if needed).

- aggdir:

  (character, default=NULL) What directory should the AGG output files
  be saved to? If `NULL`, AGG files will not be output. Note that
  `aggdir` or `llddir` (or both) must be non-NULL.

- llddir:

  (character, default=NULL) What directory should the LLD output files
  be saved to? If `NULL`, LLD files will not be output. Note that
  `aggdir` or `llddir` (or both) must be non-NULL.

- recursive:

  (logical, default=FALSE) Should files in subdirectories within `indir`
  be included?

- ...:

  Arguments passed on to
  [`os_extract`](https://jmgirard.github.io/openac/reference/os_extract.md),
  [`os_prep_audio`](https://jmgirard.github.io/openac/reference/os_prep_audio.md)

  `config`

  :   (character, default="misc/emo_large") Which configuration file
      should be used to analyze `infile`? A list of available config
      files can be generated using
      [`os_list_configs()`](https://jmgirard.github.io/openac/reference/os_list_configs.md).

  `stream`

  :   (numeric, default=0) The index of the audio stream to extract
      (ffmpeg uses zero-indexing so 0 is the first stream).

  `overwrite`

  :   Should outfile be overwritten if it already exists? It will be
      skipped otherwise, silently for a direct call. In a batch the row
      depends on whose job the preparing is: under
      [`os_prep_audio_dir()`](https://jmgirard.github.io/openac/reference/os_prep_audio_dir.md)
      it is the whole job, so the row reads `"skipped"`; under
      `os_extract_dir()` the existing file is reused and openSMILE still
      runs, so the row reads `"ok"`. Defaults to TRUE.

## Value

(Invisibly) a data frame with one row per input file, giving the paths
it was called with, its `status`, whether it `success`ed, and the
`error` message if it did not. `status` is one of `"ok"` (the operation
completed), `"skipped"` (the file was deliberately not processed) or
`"failed"` (the operation raised an error); `success` is
`status == "ok"`, so a skipped file reads `FALSE`, and `error` carries
the reason for a skipped file as well as for a failed one. A file that
fails does not abort the batch: it is warned about, recorded as
`"failed"`, and the remaining files still run. A `config` that cannot be
resolved is the exception, and is not a per-file outcome: it is wrong
for every input, so it errors before any file is touched, naming the
config, and no table is returned.

## Details

Can be optionally run in parallel by running
[`plan()`](https://future.futureverse.org/reference/plan.html)
beforehand, e.g., by calling `plan("multisession", workers = 4)`.

Can optionally output a progress bar by using
[`handlers()`](https://progressr.futureverse.org/reference/handlers.html)
beforehand, e.g., by calling `handlers("cli"); handlers(global = TRUE)`.
