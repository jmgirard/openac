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
- Short aliases exist for the low-level passthroughs: `ffm`, `ffp`, `of`,
  `os`.
- **Current** input validation is `stopifnot()` + `rlang::is_string` /
  `rlang::is_bool` predicates; failures/missing tools signal via base
  `warning()`, not `cli::cli_abort()`. (cairn's R guardrail asks that *new*
  user-facing conditions use `cli`/`rlang` — so existing code and the
  guardrail diverge; see Known issues.)

## Design Principles

_GP<n> = Guiding (tradeable with stated justification) · IP<n> = Inviolable
(hard constraint; changing one requires an explicit decision in DECISIONS.md)._

- **GP1 — Thin wrappers.** openac shapes I/O and orchestrates external
  tools; it does not reimplement a tool's internal algorithm.
- **GP2 — Batch parity.** Every single-file operation aims to have a batch
  `_dir` counterpart.
- **IP1 — No hard-coded tool paths.** External-tool locations are always
  discovered (`Sys.which`) or user-configured (`set_program`), never
  hard-coded in the package.

## Architecture

**Program discovery & configuration.** `find_program(program)` resolves an
external tool by (1) `Sys.which()` on `PATH`, then (2) a per-program config
file `<program>_location.txt` under `rappdirs::user_config_dir("openac",
"R")`. `set_program()` writes that file. Resolution returns an absolute path
(`tools::file_path_as_absolute`) or `NULL` with a `warning()`. `check_*`
report installed/working status; `install_*_{win,mac}` fetch and place
binaries.

**Calling the CLIs.** Each tool has a low-level passthrough (`ffmpeg()`,
`ffprobe()`, `openface()`, `opensmile()`) that takes a single
space-separated argument string and runs
`system2(find_<tool>(), args = arg, stdout = TRUE, stderr = TRUE)`,
returning captured output as a character vector. Typed high-level functions
(`of_extract()`, `os_extract()`, `aw_transcribe()`, …) validate named
parameters and assemble the argument string, then delegate to the
passthrough.

**Batch & parallelism.** `*_dir()` functions enumerate input files by
extension and map the single-file operation across them via `furrr`/`future`
(user sets strategy with `plan()`), surfacing progress through `progressr`
(`handlers()`). Both control functions are re-exported so users configure
them without attaching the upstream packages.

## Known issues

<!-- Running list of accepted warts; each line dated. -->
- 2026-07-11: Validation/signaling style is legacy `stopifnot()` + base
  `warning()`, diverging from cairn's `cli::cli_abort()` guardrail for new
  conditions. Migration is a candidate, not yet scheduled.
