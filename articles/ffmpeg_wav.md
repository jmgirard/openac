# Extracting audio from video with ffmpeg

This vignette shows how to pull the audio track out of a folder of video
files and write it to WAV files ready for acoustic analysis.
[`os_prep_audio()`](https://jmgirard.github.io/openac/reference/os_prep_audio.md)
wraps **ffmpeg** to extract and convert an audio stream to the format
openSMILE expects; running it across many files in parallel is a matter
of pairing it with **furrr**.

[`library`](https://rdrr.io/r/base/library.html)`(`[`openac`](https://jmgirard.github.io/openac/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`future`](https://future.futureverse.org)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`furrr`](https://github.com/futureverse/furrr)`)`

First confirm that openac can find a working ffmpeg (see
[`set_ffmpeg()`](https://jmgirard.github.io/openac/reference/set_program.md)
if not):

[`check_ffmpeg`](https://jmgirard.github.io/openac/reference/check_ffmpeg.md)`(``)`

List the input videos and derive a matching WAV path for each output:

`infiles`` ``<-`` `[`list.files`](https://rdrr.io/r/base/list.files.html)`(`` `` path ``=`` ``"Z:/DynAMoS/Stimuli/Video"``,`` `` pattern ``=`` ``"\\.mp4$"``,`` `` full.names ``=`` ``TRUE``,`` `` recursive ``=`` ``TRUE`` ``)`` `` ``# Mirror the input paths into an Audio/ folder, swapping the extension.`` ``outfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"/Stimuli/Video/"``, ``"/Stimuli/Audio/"``, ``infiles``)`` ``outfiles`` ``<-`` `[`gsub`](https://rdrr.io/r/base/grep.html)`(``"\\.mp4$"``, ``".wav"``, ``outfiles``)`

Set a parallel plan and map
[`os_prep_audio()`](https://jmgirard.github.io/openac/reference/os_prep_audio.md)
over the input/output pairs:

[`plan`](https://jmgirard.github.io/openac/reference/plan.md)`(``"multisession"``, workers ``=`` ``4``)`` `` `[`future_walk2`](https://furrr.futureverse.org/reference/future_map2.html)`(`` `` .x ``=`` ``infiles``,`` `` .y ``=`` ``outfiles``,`` `` .f ``=`` ``os_prep_audio`` ``)`

For this common “every file in a directory” case you can skip the manual
file bookkeeping entirely and let
[`os_prep_audio_dir()`](https://jmgirard.github.io/openac/reference/os_prep_audio_dir.md)
find, pair, and process the files for you. A file that fails does not
stop the run: it is warned about, recorded as `"failed"`, and the
remaining files still go through. The returned data frame records the
outcome of every file:

[`os_prep_audio_dir`](https://jmgirard.github.io/openac/reference/os_prep_audio_dir.md)`(`` `` indir ``=`` ``"Z:/DynAMoS/Stimuli/Video"``,`` `` inext ``=`` ``"mp4"``,`` `` outdir ``=`` ``"Z:/DynAMoS/Stimuli/Audio"``,`` `` recursive ``=`` ``TRUE`` ``)`

Finally, confirm the WAV files were written:

[`list.files`](https://rdrr.io/r/base/list.files.html)`(``path ``=`` ``"Z:/DynAMoS/Stimuli/Audio"``, pattern ``=`` ``"\\.wav$"``)`
