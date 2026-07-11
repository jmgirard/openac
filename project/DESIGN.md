# Design

## Purpose & Scope

<!-- Drafted by cairn-init from DESCRIPTION + a read of R/. Refine these
     lines — they are honest observations, not gospel. -->

openac provides R wrappers around external, open-source affective-computing
command-line tools so R users can run them from a single, consistent
interface. It does not implement the underlying algorithms; it locates,
installs, configures, and drives external binaries and packages, then shapes
their inputs/outputs for R workflows.

Wrapped tools: **ffmpeg** / **ffprobe** (media transcode & probe), **OpenFace**
(facial behavior extraction), **openSMILE** (acoustic feature extraction), and
**whisper** (via the `audio.whisper` package, speech transcription).

In scope: tool discovery/installation/configuration, audio/video
preparation, per-file and batch (`_dir`) extraction, parallel iteration
(via `future`/`furrr`) with progress (`progressr`).
Out of scope: reimplementing any tool's algorithm; hosting model weights.

## Function Families

- **Program management** — `check_*` (is it installed/working), `find_*`
  (locate the binary), `set_*` (record its location), `install_*_win` /
  `install_*_mac` (fetch & install), plus generic `find_program`,
  `set_program`.
- **ffmpeg / ffprobe** — `ffmpeg()`, `ffprobe()` low-level arg passthroughs
  (+ `ffm`/`ffp` aliases), `ffp_count_streams()`.
- **OpenFace (`of_*`)** — `openface()` passthrough (+ `of` alias),
  `of_extract()`, `of_extract_dir()`.
- **openSMILE (`os_*`)** — `opensmile()` passthrough (+ `os` alias), config
  helpers (`os_list_configs`, `os_check_config`), audio prep
  (`os_check_audio`, `os_prep_audio`, `os_prep_audio_dir`), extraction
  (`os_extract`, `os_extract_dir`), `os_fix_csv`.
- **whisper (`aw_*`)** — audio check/prep (`aw_check_audio`, `aw_prep_audio`,
  `aw_prep_audio_dir`), model fetch (`aw_get_model`), transcription
  (`aw_transcribe`, `aw_transcribe_dir`).
- **Re-exports** — `future::plan`, `progressr::handlers` (parallelism &
  progress control surfaced to users).

## Conventions

<!-- Observed patterns, stated as facts. Confirm/extend. -->

- Tool-family prefixes: `of_` (OpenFace), `os_` (openSMILE), `aw_`
  (audio.whisper); ffmpeg/ffprobe use full or `ffm`/`ffp` names.
- Verb families mirror the program lifecycle: `check_` / `find_` / `set_` /
  `install_`.
- Batch variants carry a `_dir` suffix and iterate with `furrr` +
  `progressr`.
- Platform-specific installers are suffixed `_win` / `_mac`.
- User-facing conditions use `cli` / `rlang`.

## Design Principles

<!-- cairn-init does NOT invent principles. Below are CANDIDATES inferred
     from the code for the user to accept, reword, or delete. Mark each as
     GP<n> (Guiding — tradeable with justification) or IP<n> (Inviolable —
     hard constraint) once confirmed. -->

- _Candidate:_ Thin wrappers — openac shapes I/O and orchestrates, it does
  not reimplement tool internals.
- _Candidate:_ Every per-file operation has a `_dir` batch counterpart.
- _Candidate:_ External-tool locations are discovered/configured, never
  hard-coded.

## Architecture

<!-- Fill in as it stabilizes: how programs are located & cached (rappdirs),
     how args are marshalled to the CLIs, how batch iteration & progress are
     wired. -->

## Known issues

<!-- Running list of accepted warts; each line dated. -->
