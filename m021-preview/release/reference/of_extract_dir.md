# Run of_extract() on multiple files in a directory

Find all video files with a specified extension in a specified directory
and then extract openface features from each.

## Usage

``` r
of_extract_dir(indir, inext, outdir, recursive = FALSE, ...)
```

## Arguments

- indir:

  (character) What directory are the input files in?

- inext:

  (character) What file extension should be looked for in `indir` (e.g.,
  "mp4" or "avi")? Matched regardless of case, so "mp4" also takes
  `.MP4` files; if that leaves two inputs deriving the same output file,
  the batch is refused rather than one silently overwriting the other.

- outdir:

  (character) What directory should the output files be created in?

- recursive:

  (logical, default=FALSE) Should files in subdirectories within `indir`
  be included?

- ...:

  Arguments passed on to
  [`of_extract`](https://jmgirard.github.io/openac/reference/of_extract.md)

  `fp2D`

  :   (logical, default=TRUE) Should the output include 2D facial
      landmark points (in pixels)?

  `fp3D`

  :   (logical, default=TRUE) Should the output include 3D facial
      landmark points (in millimeters)?

  `pdm`

  :   (logical, default=FALSE) Should the output include the parameter
      estimates of the point distribution model?

  `pose`

  :   (logical, default=TRUE) Should the output include head pose
      estimates?

  `gaze`

  :   (logical, default=TRUE) Should the output include eye gaze
      estimates?

  `aus`

  :   (logical, default=TRUE) Should the output include action unit
      estimates?

  `wild`

  :   (logical, default=FALSE) Should the model consider extended search
      regions (for challenging images)?

  `multiview`

  :   (logical, default=FALSE) Should multi-view initialisation be used
      (more robust but slower)?

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

Can optionally output a progress bar by using
[`handlers()`](https://progressr.futureverse.org/reference/handlers.html)
beforehand, e.g., by calling `handlers("cli"); handlers(global = TRUE)`.
