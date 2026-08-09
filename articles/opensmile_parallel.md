# Running opensmile in parallel

This vignette extracts acoustic features from a folder of WAV files with
**openSMILE**, in parallel via **furrr**.
[`os_extract()`](https://jmgirard.github.io/openac/reference/os_extract.md)
can write both an aggregate (functionals) CSV and a low-level-descriptor
(per-frame) CSV per file;
[`os_read()`](https://jmgirard.github.io/openac/reference/os_read.md)
later turns either into a tidy tibble.

[`library`](https://rdrr.io/r/base/library.html)`(`[`openac`](https://jmgirard.github.io/openac/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`future`](https://future.futureverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`furrr`](https://github.com/futureverse/furrr)`)`

Confirm that openac can find a working openSMILE install (see
[`set_opensmile()`](https://jmgirard.github.io/openac/reference/set_program.md)
if not, and
[`os_list_configs()`](https://jmgirard.github.io/openac/reference/os_list_configs.md)
for the available feature configs):

[`check_opensmile`](https://jmgirard.github.io/openac/reference/check_opensmile.md)`(``)`

List the input audio and derive aggregate/LLD output paths for each
file:

`infiles`` ``<-`` `[`list.files`](https://rdrr.io/r/base/list.files.html)`(`` `` path ``=`` ``"Z:/DynAMoS/Stimuli/Audio"``,`` `` pattern ``=`` ``"\\.wav$"``,`` `` full.names ``=`` ``TRUE``,`` `` recursive ``=`` ``TRUE`` ``)`` `` ``aggfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"/Stimuli/Audio/"``, ``"/Features/opensmile/"``, ``infiles``)`` ``aggfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"\\.wav$"``, ``"_agg.csv"``, ``aggfiles``)`` ``lldfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"_agg\\.csv$"``, ``"_lld.csv"``, ``aggfiles``)`

Set a parallel plan and map
[`os_extract()`](https://jmgirard.github.io/openac/reference/os_extract.md)
over the input/output triples. The `config` argument (forwarded to every
call) selects the openSMILE feature set:

[`plan`](https://jmgirard.github.io/openac/reference/plan.md)`(``"multisession"``, workers ``=`` ``4``)`` `` `[`future_pwalk`](https://furrr.futureverse.org/reference/future_map2.html)`(`` `` .l ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``infile ``=`` ``infiles``, aggfile ``=`` ``aggfiles``, lldfile ``=`` ``lldfiles``)``,`` `` .f ``=`` ``os_extract``,`` `` config ``=`` ``"egemaps/v02/eGeMAPSv02"`` ``)`

For the whole-directory case,
[`os_extract_dir()`](https://jmgirard.github.io/openac/reference/os_extract_dir.md)
finds and processes every matching file for you. A file that fails does
not stop the run: it is warned about, recorded as `"failed"`, and the
remaining files still go through. The returned data frame records the
outcome of every file, so you can find and re-run just the failures:

[`os_extract_dir`](https://jmgirard.github.io/openac/reference/os_extract_dir.md)`(`` `` indir ``=`` ``"Z:/DynAMoS/Stimuli/Audio"``,`` `` inext ``=`` ``"wav"``,`` `` aggdir ``=`` ``"Z:/DynAMoS/Features/opensmile"``,`` `` config ``=`` ``"egemaps/v02/eGeMAPSv02"``,`` `` recursive ``=`` ``TRUE`` ``)`

Confirm the feature files were written, and read one into a tidy tibble:

[`list.files`](https://rdrr.io/r/base/list.files.html)`(``path ``=`` ``"Z:/DynAMoS/Features/opensmile"``, pattern ``=`` ``"\\.csv$"``)`` `` ``feats`` ``<-`` `[`os_read`](https://jmgirard.github.io/openac/reference/os_read.md)`(``"Z:/DynAMoS/Features/opensmile/example_agg.csv"``)`
