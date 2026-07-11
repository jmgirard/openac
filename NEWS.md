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
