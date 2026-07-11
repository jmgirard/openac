# Design

## Purpose & Scope

openac provides R wrappers around external, open-source affective-computing
tools so researchers can run them from a single, consistent R interface. The
primary audience is **affective-science researchers** (including the
maintainer's lab and collaborators) who are comfortable in R but not at the
command line — the package exists to spare them the terminal.

Wrapped tools: **ffmpeg** / **ffprobe** (media transcode & probe), **OpenFace**
(facial behavior extraction), **openSMILE** (acoustic feature extraction), and
**whisper** (via the `audio.whisper` package, speech transcription). The
roster grows **opportunistically** — no fixed roadmap, but a tool earns a
wrapper family when it is (a) open-source and freely available, (b) extracts
or processes affective/behavioral signals, and (c) drivable non-interactively
(CLI or R-callable). Modern ML tools (e.g., HuggingFace models) are of
long-term interest (see ROADMAP candidates).

In scope: tool discovery/installation/configuration, audio/video
preparation, per-file and batch (`_dir`) extraction, parallel iteration
(via `future`/`furrr`) with progress (`progressr`), and — direction, mostly
unbuilt — **reading tool outputs into tidy R data frames** (`os_fix_csv` is
the seed; see ROADMAP candidates).
Out of scope: reimplementing any tool's algorithm; hosting model weights.

Platforms: all three (Windows/macOS/Linux) **where possible** — wrappers
(find/check/run) should work everywhere; `install_*` helpers are
best-effort per platform. Distribution is GitHub-only today with **CRAN as
the eventual goal**, so code converges on CRAN standards as it goes.

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

- Tool-family prefixes: `of_` (OpenFace), `os_` (openSMILE), `aw_`
  (audio.whisper); ffmpeg/ffprobe use full or `ffm`/`ffp` names.
- Verb families mirror the program lifecycle: `check_` / `find_` / `set_` /
  `install_`.
- Batch variants carry a `_dir` suffix and iterate with `furrr` +
  `progressr`.
- Platform-specific installers are suffixed `_win` / `_mac`.
- Short aliases exist for the low-level passthroughs: `ffm`, `ffp`, `of`,
  `os`.
- Conditions: **new code uses `cli::cli_abort()`/`cli_warn()`; legacy
  `stopifnot()` + base `warning()` migrates opportunistically when touched**
  (no wholesale conversion milestone). Better messages matter for a
  non-technical audience.
- Pre-1.0, the exported API may **break freely** — no deprecation ceremony
  until 1.0 (explicit waiver, D-002).

## Design Principles

_GP<n> = Guiding (tradeable with stated justification) · IP<n> = Inviolable
(hard constraint; changing one requires an explicit decision in DECISIONS.md)._

- **GP1 — Thin wrappers.** openac never re-derives the signals themselves
  (no face tracking, feature extraction, or transcription of its own — that
  is the tools' job), but parsing, cleaning, and aggregating tool *outputs*
  is squarely in scope.
- **GP2 — Batch parity.** Every single-file operation has a batch
  counterpart with parallelism (`future`/`furrr`) and progress
  (`progressr`). The principle is the capability, not the API shape — the
  current `_dir(indir, inext)` idiom may evolve (e.g., toward vectorized
  path inputs) under D-002.
- **GP3 — Cross-platform where possible.** Wrappers (find/check/run) work on
  Windows/macOS/Linux; `install_*` helpers are best-effort per platform.
- **GP4 — Lean dependencies.** The current Imports list is acceptable but
  shrinks where easy; new Imports need real justification (and, per the
  guardrails, a question-gate + D-entry).
- **GP5 — Transparent calls.** The exact external command a high-level
  function constructs is discoverable/reportable by the user (methods
  sections, version-drift debugging). Retrofit opportunistically.
- **GP6 — Resilient batches.** Batch operations skip-and-report per-file
  failures rather than aborting the run — a crash on file 412 of 500
  overnight is the failure mode to design against.
- **IP1 — No hard-coded paths, no surprise writes.** External-tool locations
  are always discovered (`Sys.which`) or user-configured (`set_program`),
  never hard-coded; openac writes only to user-specified locations or its
  own `rappdirs` config/cache dirs.
- **IP2 — Source media are sacrosanct.** No code path modifies or deletes a
  user's original media files; transformations always write new outputs.
  (In-place edits of openac-*generated* intermediates — e.g., `os_fix_csv`
  rewriting an openSMILE output CSV — fall outside this rule but stay rare
  and explicit.)
- **IP3 — No silent data egress.** Participant data never leaves the user's
  machine without explicit, unmistakable opt-in; processing is local by
  default. Cloud-backed wrappers are permissible down the road *only*
  behind such opt-in (IRB/consent constraints). Network access otherwise
  fetches only tools/models, at user request.

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
- 2026-07-11: Legacy `stopifnot()` + base `warning()` style persists until
  code is touched (opportunistic migration to `cli` — see Conventions).
- 2026-07-11: **External-tool version drift** — wrappers assume particular
  tool versions/flag sets; upstream updates can silently break arg
  construction. No version pinning or detection yet.
- 2026-07-11: **Messy real-world media** — recordings with missing audio
  streams, odd codecs, or multiple streams keep surprising the pipeline
  (recent fixes: stream counting, audio-less files, video-vs-audio checks).
- 2026-07-11: **OpenFace availability** — effectively unmaintained upstream
  and hard to install (especially mac/Linux); a structural risk for the
  `of_*` family.
- 2026-07-11: **Windows-biased testing** — most real use has been on
  Windows; mac/Linux paths are lightly exercised and may have quiet
  breakage.
- 2026-07-11: Essentially no automated tests (one empty stub); recent bug
  fixes shipped without regression tests.
- 2026-07-11: GP5 unmet — high-level functions build their command strings
  internally with no way for users to inspect/report them; retrofit when
  touched.
- 2026-07-11: GP6 unevenly met — some `_dir` functions skip bad files
  (audio-less inputs), but resilience is ad hoc, not a designed contract.
