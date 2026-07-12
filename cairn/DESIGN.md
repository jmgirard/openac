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
(via `future`/`furrr`) with progress (`progressr`), and **reading tool outputs
into tidy R data frames** via the `<tool>_read()` family (`os_read`, `of_read`,
`aw_read`; see the tidy-reader contract under Conventions).
Out of scope: reimplementing any tool's algorithm; hosting model weights.

Platforms: all three (Windows/macOS/Linux) **where possible** — wrappers
(find/check/run) should work everywhere; `install_*` helpers are
best-effort per platform. Distribution is GitHub-only today with **CRAN as
the eventual goal**, so code converges on CRAN standards as it goes.
CRAN submission is gated by a **quality bar, not a date or the 1.0 freeze**:
(1) a testing contract for the binary-dependent wrappers, and (2) a
CRAN-legal resolution for `audio.whisper` (the `Remotes:` field cannot ship;
Additional_repositories vs. wrapping whisper.cpp directly is deliberately
deferred to submission time — see ROADMAP). Until that decision, deepening
openac's reliance on `audio.whisper` internals is done with eyes open.

## Function Families

- **Program management** — `check_*` (is it installed/working), `find_*`
  (locate the binary), `set_*` (record its location), `install_*_win` /
  `install_*_mac` (fetch & install), plus generic `find_program`,
  `set_program`.
- **ffmpeg / ffprobe** — `ffmpeg()`, `ffprobe()` low-level arg passthroughs
  (+ `ffm`/`ffp` aliases), `ffp_count_streams()`.
- **OpenFace (`of_*`)** — `openface()` passthrough (+ `of` alias),
  `of_extract()`, `of_extract_dir()`, output reading (`of_read()`).
- **openSMILE (`os_*`)** — `opensmile()` passthrough (+ `os` alias), config
  helpers (`os_list_configs`, `os_check_config`), audio prep
  (`os_check_audio`, `os_prep_audio`, `os_prep_audio_dir`), extraction
  (`os_extract`, `os_extract_dir`), output reading (`os_read`), `os_fix_csv`.
- **whisper (`aw_*`)** — audio check/prep (`aw_check_audio`, `aw_prep_audio`,
  `aw_prep_audio_dir`), model fetch (`aw_get_model`), transcription
  (`aw_transcribe`, `aw_transcribe_dir`), transcript reading (`aw_read`).
- **Tidy readers (`<tool>_read`)** — `os_read`, `of_read`, `aw_read` turn a
  tool's raw output into a tidy tibble under one shared contract (Conventions).
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
- **Tidy-reader contract** (`<tool>_read`, D-008): a reader turns one tool's
  raw output into a tidy tibble. It **accepts every form that output natively
  exists in** — a `file` path for file-only tools (`os_read`, `of_read`); the
  in-memory object *plus* the `.rds`/`.csv` sidecars the wrapper writes, as
  `x`, for R-native tools (`aw_read`) — and all accepted forms yield identical
  output. It **passes through every data-bearing column** (dropping only
  redundant re-encodings, e.g. whisper's `segment_offset`), does **lossless
  type-parsing only** (never filtering, aggregating, or deriving), and reports
  **time as numeric seconds** (`frameTime`, `timestamp`, `from`/`to`). Tool
  column names are kept **verbatim** (mechanical whitespace cleanup only);
  each reader states its row grain in its first roxygen sentence.
- **Output-overwrite default is `TRUE`** across functions that write derived
  files (`os_prep_audio`, `aw_prep_audio`, …): re-runs regenerate outputs;
  a stale output silently poisoning an analysis is the failure mode to
  avoid. IP2 protects *source* media — these are openac-generated
  derivatives. (`overwrite = FALSE` remains available and doubles as
  resume-a-crashed-batch semantics.)
- Conditions: **new code uses `cli::cli_abort()`/`cli_warn()`; legacy
  `stopifnot()` + base `warning()` migrates opportunistically when touched**
  (no wholesale conversion milestone). Better messages matter for a
  non-technical audience.
- Pre-1.0, the exported API may **break freely** — no deprecation ceremony
  until 1.0 (explicit waiver, D-002).

## Design Principles

_IP<n> = Inviolable (hard constraint; changing one requires an explicit
decision in DECISIONS.md) · GP<n> = Guiding (tradeable with stated
justification). IP block first, then GPs; numbers never reused._

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
- **GP1 — Thin wrappers.** openac never re-derives the signals themselves
  (no face tracking, feature extraction, or transcription of its own — that
  is the tools' job), but parsing, cleaning, and aggregating tool *outputs*
  is squarely in scope. Post-read helpers (aggregation/derivation) each
  earn their place as a ROADMAP candidate; the tidy reader is the
  guaranteed surface (D-009).
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
- **GP7 — Two-layer testing.** Binary-dependent code is tested at two
  layers: command construction is verified everywhere by mocking the
  `system2` boundary (an inspectable command is a testable command — GP5),
  and real tool invocations run locally behind skip-if-binary-missing +
  `skip_on_cran` gates. Retrofit is opportunistic; scheduling is a ROADMAP
  matter.
- **GP8 — Report what ran.** High-level functions make what actually ran
  recoverable — tool identity, version, and the constructed command — for
  methods-section reporting and version-drift debugging. The runtime
  counterpart to GP5. (Implementation is a ROADMAP candidate.)
- **GP9 — Probe before compute.** High-level operations validate input
  media with a cheap probe (`ffprobe`-based stream/type checks) and fail or
  skip with a message naming the file and the defect, before the expensive
  tool runs. Complements GP6: GP6 makes batches survive bad files; GP9
  makes bad files diagnosable.

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
  `of_*` family. Posture: keep fully supported while it works (frozen
  upstream ≈ zero drift risk); successor scouting is a ROADMAP candidate.
- 2026-07-11: **Windows-biased testing** — most real use has been on
  Windows; mac/Linux paths are lightly exercised and may have quiet
  breakage.
- 2026-07-11: The readers have fixture-backed tests (M01–M03), but the
  binary-dependent wrappers (extract/prep/install — most of the package)
  have none; recent bug fixes there shipped without regression tests.
- 2026-07-11: **OneDrive model URLs** — `install_openface_win` downloads
  model files from hard-coded OneDrive links with embedded authkeys
  (`programs_install.R`); links of that shape die silently. A time bomb in
  the install path.
- 2026-07-11: **audio.whisper volatility** — the `aw_*` family rests on a
  GitHub-only upstream package (≥ 0.4.1) whose API and maintenance cadence
  openac does not control; upstream changes could break it without warning.
- 2026-07-11: **Config is global-only** — `set_program` writes one
  `rappdirs` config per tool per machine; no per-project override, so two
  projects needing different tool builds fight over the same file.
- 2026-07-11: GP5 unmet — high-level functions build their command strings
  internally with no way for users to inspect/report them; retrofit when
  touched.
- 2026-07-11: GP6 unevenly met — some `_dir` functions skip bad files
  (audio-less inputs), but resilience is ad hoc, not a designed contract.
