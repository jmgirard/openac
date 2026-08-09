# Read OpenFace output into a tidy tibble

Read an OpenFace output CSV (as written by
[`of_extract()`](https://jmgirard.github.io/openac/reference/of_extract.md))
into a wide [tibble](https://tibble.tidyverse.org/reference/tibble.html)
with one row per detected face per frame (OpenFace uses a multi-face
model, so a frame with several faces yields several rows sharing a
`frame` but differing in `face_id`). Metadata columns (`frame`,
`face_id`, `timestamp`, `confidence`, `success`) come first, followed by
whichever feature blocks OpenFace emitted (gaze, head pose, 2D/3D facial
landmarks, PDM parameters, and action-unit intensities `AU*_r` and
presences `AU*_c`), all passed through as-is.

## Usage

``` r
of_read(file)
```

## Arguments

- file:

  (character) Path to an OpenFace output CSV.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per detected face per frame and one column per OpenFace metadata
field and feature.

## Details

OpenFace writes space-padded column headers (e.g. `" confidence"`); the
leading/trailing whitespace is stripped from the column names.

## See also

[`of_extract()`](https://jmgirard.github.io/openac/reference/of_extract.md),
which produces the output file.

## Examples

``` r
if (FALSE) { # \dontrun{
of_extract("video.mp4", outfile = "video.csv")
faces <- of_read("video.csv")
} # }
```
