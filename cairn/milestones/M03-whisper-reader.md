# M03: whisper transcript tidy reader (`aw_read`)

- **Status:** blocked   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M01   <!-- inherits reader-family conventions (naming, wide tibble, tibble Import) -->
- **Branch/PR:** m03-whisper-reader   <!-- PR URL once opened -->

## Goal

Add an exported `aw_read()` that turns an `audio.whisper` transcription
result into a tidy tibble — one row per segment — following the M01 pattern.

## Scope

**In:**
- New exported `aw_read(x)` accepting the object `aw_transcribe()` returns
  (its `$data` holds `segment`, `from`, `to`, `text`) **or** a path to the
  `.rds` (full object) / `.csv` (`out$data`) that `aw_transcribe()` writes
  (R/use_whisper.R:367,378).
- Returns a tibble, one row per segment: `segment` (int), `from`/`to`
  (numeric seconds), `text` (chr, verbatim).
- Parse whisper `HH:MM:SS.mmm` timestamps to numeric seconds.
- Empty transcript (no segments) → zero-row tibble with the right columns,
  not an error.
- Errors via `cli::cli_abort()`.

**Out:**
- Token-level output (`$tokens`) → candidate row.
- Speaker diarization columns → candidate (only if upstream provides them).
- openSMILE reader → M01; OpenFace reader → M02.

## Acceptance criteria

- [x] `aw_read()` exported; returns a tibble with one row per segment.
- [x] `from`/`to` converted to numeric seconds — fixture `"00:00:01.500"`
      yields `1.5`; `text` preserved exactly (incl. leading spaces if present).
- [x] Same tibble from the in-memory result object, its `.rds`, and its
      `.csv` form (all three round-trip to identical output).
- [x] Empty-transcript input → 0-row tibble with columns
      `segment`, `from`, `to`, `text` (no error).
- [x] `cli::cli_abort()` fires on: wrong-type input; missing file for a path
      argument.
- [x] `devtools::test()` green; `devtools::check()` clean — 0E/0W/0N apart
      from the 2 pre-existing vignette warnings (M05's scope; see 2026-07-11
      amendment). Verified with vignettes off, as in M04.

## Tasks

- [x] Build a fixture mirroring the `audio.whisper` result structure
      (class `whisper_transcription`; `$data` incl. `segment_offset`) via a
      pure-R constructor in the test file, materialised to tempfile
      `.rds`/`.csv` per test (no committed binary; no `audio.whisper` install).
- [x] Write failing tests (`tests/testthat/test-whisper-read.R`): segment
      rows, time parsing, object/rds/csv parity, empty transcript, errors,
      and that `segment_offset` is dropped.
- [x] Implement `aw_read()` in `R/use_whisper.R`: dispatch on input
      (object vs `.rds` vs `.csv`), reach `$data`, parse times, coerce to
      tibble; `cli::cli_abort()` on bad input.
- [x] Roxygen doc + `@examples`; `Rscript -e 'devtools::document()'`.
- [x] `devtools::test()` green (92 pass); `devtools::check()` clean (vignettes
      off) — NEWS entry added for `aw_read()`.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan.
- 2026-07-11: started; branch m03-whisper-reader cut from main.
- 2026-07-11: verified `audio.whisper` `predict.whisper()` output against its
  source (bnosac/audio.whisper `R/whisper.R`). `$data` columns are `segment`,
  `segment_offset`, `text`, `from`, `to` (+ `speaker` if diarized) — the plan
  omitted `segment_offset`. `from`/`to` = `format(POSIXct + start/1000,
  "%H:%M:%OS")` → `"HH:MM:SS.mmm"`. Reference note added.
- 2026-07-11: amendment (minor) — fixture built in a testthat helper (pure-R
  constructor incl. `segment_offset`, class `whisper_transcription`), written
  to tempfile `.rds`/`.csv` per test, rather than committing a binary `.rds`
  blob. More transparent oracle; also tests that `aw_read` drops the extra
  `segment_offset` column.
- 2026-07-11: amendment (minor) — final criterion "check() clean" scoped to
  "clean apart from the 2 vignette warnings (M05)". Forced by the M04/M05
  vignette split that post-dates M03's planning; mirrors M04's user-approved
  treatment. M03 itself adds 0 new check findings.
- 2026-07-11: implemented `aw_read()` (+ internal `aw_read_data`,
  `aw_parse_timestamp`); 25 new tests, all green; suite 92 pass;
  `check()` (vignettes off) 0E/0W/0N. Status → review.
- 2026-07-11: blocked on RB01 (tidy-reader family API contract, Fable
  escalation). Brief created on this branch (not main) so M03's status and the
  RR-ingested decisions stay coherent while M03 is unmerged. Held from merge
  until the RR is ingested — the RR may recommend changing `aw_read`
  (columns/timestamps).

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

- Inherits D-004 (naming) and D-005 (`tibble` Import) from M01.

## Review
<!-- filled by /milestone-review -->
