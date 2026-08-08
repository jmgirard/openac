# openac (development version)

* The low-level wrappers `ffmpeg()`, `ffprobe()`, `openface()` and `opensmile()`
  (and their aliases `ffm()`, `ffp()`, `of()` and `os()`) now fail with an error
  naming the program when it cannot be found, instead of the low-level error
  that previously surfaced from path resolution.

* `find_program()` (and `find_ffmpeg()`, `find_ffprobe()`, `find_openface()` and
  `find_opensmile()`) now warn and return `NULL` when a program cannot be found,
  instead of failing with an error. `check_ffmpeg()`, `check_ffprobe()`,
  `check_openface()` and
  `check_opensmile()` correspondingly return `FALSE` in that case.

* `find_program()` now resolves a location recorded by `set_program()` even when
  that location is a bare program name found on the `PATH`.

* The documented return value of `set_program()` now matches what it returns.

# openac 0.1.0

First public release. openac wraps open-source affective-computing tools —
ffmpeg/ffprobe, OpenFace, openSMILE, and Whisper (via audio.whisper) — behind
one consistent R interface: tool discovery/installation, audio/video
preparation, single-file and batch (`_dir`) extraction with parallelism
(future/furrr) and progress (progressr), and tidy readers for the tool outputs.

Reading tool outputs into tidy tibbles:

* `os_read()` reads openSMILE CSV output — either an aggregate (functionals)
  file or a per-frame low-level descriptor (LLD) file — into a tidy tibble,
  auto-detecting the delimiter and preserving openSMILE's feature names.
* `of_read()` reads an OpenFace output CSV into a tidy tibble (one row per
  detected face per frame), trimming the whitespace OpenFace pads its column
  headers with.
* `aw_read()` turns a Whisper transcription — the object from `aw_transcribe()`,
  or its `.rds`/`.csv` output — into a tidy tibble with one row per segment,
  parsing the `HH:MM:SS.mmm` timestamps to numeric seconds and keeping a
  `speaker` column for diarized transcripts.
