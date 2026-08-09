# Running openface in parallel

This vignette extracts facial-behavior features from a folder of videos
with **OpenFace**, running many files at once via **furrr**.
[`of_extract()`](https://jmgirard.github.io/openac/reference/of_extract.md)
writes one CSV of frame-level features per video;
[`of_read()`](https://jmgirard.github.io/openac/reference/of_read.md)
later turns any such CSV into a tidy tibble.

[`library`](https://rdrr.io/r/base/library.html)`(`[`openac`](https://jmgirard.github.io/openac/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`future`](https://future.futureverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`furrr`](https://github.com/futureverse/furrr)`)`

Confirm that openac can find a working OpenFace install (see
[`set_openface()`](https://jmgirard.github.io/openac/reference/set_program.md)
if not):

[`check_openface`](https://jmgirard.github.io/openac/reference/check_openface.md)`(``)`

List the input videos and derive a matching CSV path for each output:

`infiles`` ``<-`` `[`list.files`](https://rdrr.io/r/base/list.files.html)`(`` `` path ``=`` ``"Z:/DynAMoS/Stimuli/Video"``,`` `` pattern ``=`` ``"\\.mp4$"``,`` `` full.names ``=`` ``TRUE``,`` `` recursive ``=`` ``TRUE`` ``)`` `` ``outfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"/Stimuli/Video/"``, ``"/Features/openface/"``, ``infiles``)`` ``outfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"\\.mp4$"``, ``".csv"``, ``outfiles``)`

Set a parallel plan and map
[`of_extract()`](https://jmgirard.github.io/openac/reference/of_extract.md)
over the input/output pairs. Extra arguments (here the
point-distribution model plus wild and multi-view modes) are forwarded
to every call:

[`plan`](https://jmgirard.github.io/openac/reference/plan.md)`(``"multisession"``, workers ``=`` ``4``)`` `` `[`future_walk2`](https://furrr.futureverse.org/reference/future_map2.html)`(`` `` .x ``=`` ``infiles``,`` `` .y ``=`` ``outfiles``,`` `` .f ``=`` ``of_extract``,`` `` pdm ``=`` ``TRUE``,`` `` wild ``=`` ``TRUE``,`` `` multiview ``=`` ``TRUE`` ``)`

For the whole-directory case,
[`of_extract_dir()`](https://jmgirard.github.io/openac/reference/of_extract_dir.md)
finds and processes every matching file for you. A file that fails does
not stop the run: it is warned about, recorded as `"failed"`, and the
remaining files still go through. The returned data frame records the
outcome of every file, so you can find and re-run just the failures:

[`of_extract_dir`](https://jmgirard.github.io/openac/reference/of_extract_dir.md)`(`` `` indir ``=`` ``"Z:/DynAMoS/Stimuli/Video"``,`` `` inext ``=`` ``"mp4"``,`` `` outdir ``=`` ``"Z:/DynAMoS/Features/openface"``,`` `` recursive ``=`` ``TRUE``,`` `` pdm ``=`` ``TRUE``,`` `` wild ``=`` ``TRUE``,`` `` multiview ``=`` ``TRUE`` ``)`

Confirm the feature files were written, and read one into a tidy tibble:

[`list.files`](https://rdrr.io/r/base/list.files.html)`(``path ``=`` ``"Z:/DynAMoS/Features/openface"``, pattern ``=`` ``"\\.csv$"``)`` `` ``faces`` ``<-`` `[`of_read`](https://jmgirard.github.io/openac/reference/of_read.md)`(``"Z:/DynAMoS/Features/openface/example.csv"``)`
