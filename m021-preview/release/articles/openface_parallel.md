# Running openface in parallel

This vignette extracts facial-behavior features from a folder of videos
with **OpenFace**, running many files at once via **furrr**.
[`of_extract()`](https://jmgirard.github.io/openac/reference/of_extract.md)
writes one CSV of frame-level features per video;
[`of_read()`](https://jmgirard.github.io/openac/reference/of_read.md)
later turns any such CSV into a tidy tibble.

``` r

library(openac)
library(future)
library(furrr)
```

Confirm that openac can find a working OpenFace install (see
[`set_openface()`](https://jmgirard.github.io/openac/reference/set_program.md)
if not):

``` r

check_openface()
```

List the input videos and derive a matching CSV path for each output:

``` r

infiles <- list.files(
  path = "Z:/DynAMoS/Stimuli/Video",
  pattern = "\\.mp4$",
  full.names = TRUE,
  recursive = TRUE
)

outfiles <- gsub("/Stimuli/Video/", "/Features/openface/", infiles)
outfiles <- gsub("\\.mp4$", ".csv", outfiles)
```

Set a parallel plan and map
[`of_extract()`](https://jmgirard.github.io/openac/reference/of_extract.md)
over the input/output pairs. Extra arguments (here the
point-distribution model plus wild and multi-view modes) are forwarded
to every call:

``` r

plan("multisession", workers = 4)

future_walk2(
  .x = infiles,
  .y = outfiles,
  .f = of_extract,
  pdm = TRUE,
  wild = TRUE,
  multiview = TRUE
)
```

For the whole-directory case,
[`of_extract_dir()`](https://jmgirard.github.io/openac/reference/of_extract_dir.md)
finds and processes every matching file for you. A file that fails does
not stop the run: it is warned about, recorded as `"failed"`, and the
remaining files still go through. The returned data frame records the
outcome of every file, so you can find and re-run just the failures:

``` r

of_extract_dir(
  indir = "Z:/DynAMoS/Stimuli/Video",
  inext = "mp4",
  outdir = "Z:/DynAMoS/Features/openface",
  recursive = TRUE,
  pdm = TRUE,
  wild = TRUE,
  multiview = TRUE
)
```

Confirm the feature files were written, and read one into a tidy tibble:

``` r

list.files(path = "Z:/DynAMoS/Features/openface", pattern = "\\.csv$")

faces <- of_read("Z:/DynAMoS/Features/openface/example.csv")
```
