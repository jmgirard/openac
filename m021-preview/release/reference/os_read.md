# Read openSMILE output into a tidy tibble

Read an openSMILE output CSV — either an aggregate/functionals file
(from `-csvoutput`, one row) or a low-level descriptor file (from
`-lldcsvoutput`, one row per frame) — into a wide
[tibble](https://tibble.tidyverse.org/reference/tibble.html). There is
one row per observation and one column per feature, alongside the
openSMILE metadata columns (`name`, and `frameTime` for LLD output).

## Usage

``` r
os_read(file)
```

## Arguments

- file:

  (character) Path to an openSMILE output CSV, as written by
  [`os_extract()`](https://jmgirard.github.io/openac/reference/os_extract.md)
  (its `aggfile` or `lldfile`).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per observation and one column per openSMILE metadata field and
feature.

## Details

The delimiter is detected automatically, so both the native
semicolon-delimited output openSMILE writes and the comma-delimited form
produced by `os_fix_csv()` are accepted. Feature names are preserved
verbatim, including non-syntactic names such as `pcm_fftMag_mfcc[1]`.

## See also

[`os_extract()`](https://jmgirard.github.io/openac/reference/os_extract.md),
which produces the output files.

## Examples

``` r
if (FALSE) { # \dontrun{
os_extract("audio.wav", aggfile = "agg.csv", lldfile = "lld.csv")
agg <- os_read("agg.csv")
lld <- os_read("lld.csv")
} # }
```
