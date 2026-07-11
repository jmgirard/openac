# M03: whisper transcript tidy reader (`aw_read`)

- **Status:** planned   <!-- mirror; project/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M01   <!-- inherits reader-family conventions (naming, wide tibble, tibble Import) -->
- **Branch/PR:** —   <!-- m03-whisper-reader; PR URL once opened -->

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

- [ ] `aw_read()` exported; returns a tibble with one row per segment.
- [ ] `from`/`to` converted to numeric seconds — fixture `"00:00:01.500"`
      yields `1.5`; `text` preserved exactly (incl. leading spaces if present).
- [ ] Same tibble from the in-memory result object, its `.rds`, and its
      `.csv` form (all three round-trip to identical output).
- [ ] Empty-transcript input → 0-row tibble with columns
      `segment`, `from`, `to`, `text` (no error).
- [ ] `cli::cli_abort()` fires on: wrong-type input; missing file for a path
      argument.
- [ ] `devtools::test()` green; `devtools::check()` clean.

## Tasks

- [ ] Build a fixture: a minimal list mirroring the `audio.whisper` result
      structure (`$data` = data.frame of `segment`, `from`, `to`, `text`),
      saved as `.rds`, plus a `.csv` twin of `$data`, under
      `tests/testthat/fixtures/`. (No `audio.whisper` install needed —
      `aw_read` only reshapes structure; guard any real-package path with
      `skip_if_not_installed()`.)
- [ ] Write failing tests (`tests/testthat/test-whisper-read.R`): segment
      rows, time parsing, object/rds/csv parity, empty transcript, errors.
- [ ] Implement `aw_read()` in `R/use_whisper.R`: dispatch on input
      (object vs `.rds` vs `.csv`), reach `$data`, parse times, coerce to
      tibble; `cli::cli_abort()` on bad input.
- [ ] Roxygen doc + `@examples`; `Rscript -e 'devtools::document()'`.
- [ ] `devtools::test()` green; `devtools::check()` clean.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan.

## Decisions
<!-- milestone-local; promote cross-cutting ones to project/DECISIONS.md -->

- Inherits D-004 (naming) and D-005 (`tibble` Import) from M01.

## Review
<!-- filled by /milestone-review -->
