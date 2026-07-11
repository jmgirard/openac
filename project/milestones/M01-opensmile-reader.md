# M01: openSMILE tidy reader (`os_read`)

- **Status:** in-progress   <!-- mirror; project/ROADMAP.md is the authority -->
- **Priority:** high   <!-- high | normal | low -->
- **Depends on:** —
- **Branch/PR:** m01-opensmile-reader   <!-- PR URL once opened -->

## Goal

Add an exported `os_read()` that parses openSMILE CSV output into a wide,
tidy tibble — establishing the reader family's conventions (D-004, D-005).

## Scope

**In:**
- New exported `os_read(file)` returning a wide tibble (`tibble::is_tibble()`):
  one row per observation, one column per feature, plus openSMILE metadata
  columns (`name`; `frameTime` for LLD output).
- Handles both openSMILE output kinds: aggregate/functionals (`-csvoutput`,
  one row) and LLD (`-lldcsvoutput`, one row per frame).
- Robust to delimiter: reads both native openSMILE `;`-delimited output and
  the comma-normalized form produced by `os_fix_csv` (auto-detected).
- Adds `tibble` to Imports (D-005).
- Errors via `cli::cli_abort()` (new-code convention).

**Out:**
- Changing or removing `os_fix_csv` / `os_extract`'s on-disk normalization →
  candidate row (revisit once `os_read` proves the on-disk rewrite redundant).
- OpenFace reader → M02; whisper reader → M03.
- A `long = TRUE` pivot option → candidate (return shape is wide-only per
  the plan gate).
- pkgdown reference row → N/A (no `_pkgdown.yml` in repo yet).

## Acceptance criteria

- [x] `os_read()` exported; returns an object where `tibble::is_tibble()` is
      `TRUE`.
- [x] Aggregate fixture → one row; `name` column present; feature columns
      numeric; spot values equal the hand-built fixture's known values.
- [x] LLD fixture → one row per frame (row count matches fixture);
      `frameTime` numeric and non-decreasing; feature columns numeric; spot
      values match.
- [x] Same tibble is returned from the native `;`-delimited fixture and its
      comma-normalized twin (delimiter auto-detection).
- [x] `cli::cli_abort()` fires on: missing file; unreadable/garbage input
      (empty file) + non-string input.
- [x] `tibble` present in DESCRIPTION Imports; `devtools::document()` re-run;
      NAMESPACE updated by roxygen (not hand-edited).
- [ ] `devtools::test()` green; `devtools::check()` clean (0 errors, 0
      warnings; notes explained). _(test green; check pending)_

## Tasks

- [x] Add `tibble` to Imports in DESCRIPTION; `Rscript -e 'devtools::document()'`.
- [x] Create `tests/testthat/fixtures/` with a small aggregate and a small LLD
      openSMILE CSV (native `;` format, known values); comma-normalized twin
      generated in-test via `os_fix_csv`. No `.Rbuildignore` entry — fixtures
      ship with the tests.
- [x] Write failing tests (`tests/testthat/test-opensmile-read.R`): shape,
      row counts, column types, spot values, delimiter parity, error branches.
- [x] Implement `os_read()` in `R/use_opensmile.R` (after `os_fix_csv`):
      sniff delimiter from header line, read, coerce to tibble;
      `cli::cli_abort()` on bad input.
- [x] Roxygen doc for `os_read` (params, `@return` a tibble, `@examples`);
      `Rscript -e 'devtools::document()'`.
- [ ] `devtools::test()` green; `devtools::check()` clean.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan.
- 2026-07-11: implemented `os_read()` (delimiter-sniffing, quote/`check.names`
  robust) + hand-built agg/LLD fixtures + 8 tests (24 assertions, all green);
  added `tibble` to Imports (D-005). `check()` pending.

## Decisions
<!-- milestone-local; promote cross-cutting ones to project/DECISIONS.md -->

- Reader-family naming (`<tool>_read`) → D-004; `tibble` Import → D-005 (both
  cross-cutting, recorded in project/DECISIONS.md).

## Review
<!-- filled by /milestone-review -->
