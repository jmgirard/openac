# M02: OpenFace tidy reader (`of_read`)

- **Status:** planned   <!-- mirror; project/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M01   <!-- inherits reader-family conventions (naming, wide tibble, tibble Import) -->
- **Branch/PR:** —   <!-- m02-openface-reader; PR URL once opened -->

## Goal

Add an exported `of_read()` that parses an OpenFace output CSV into a wide,
tidy tibble (one row per frame), following the M01 reader-family pattern.

## Scope

**In:**
- New exported `of_read(file)` returning a wide tibble: one row per frame,
  metadata columns (`frame`, `face_id`, `timestamp`, `confidence`, `success`)
  plus all feature columns OpenFace emits (gaze, pose, 2D/3D landmarks, PDM,
  AU intensities `AU*_r` and presences `AU*_c`) passed through as-is.
- Strip leading/trailing whitespace from column names (OpenFace writes
  space-padded headers, e.g. `" confidence"`).
- Errors via `cli::cli_abort()`.

**Out:**
- Selecting/subsetting feature blocks (gaze-only, AU-only, …) → candidate row.
- Reconciling with `of_extract`'s `fp2D`/`aus`/… toggles → candidate.
- openSMILE reader → M01; whisper reader → M03.

## Acceptance criteria

- [ ] `of_read()` exported; returns a tibble.
- [ ] Fixture OpenFace CSV → one row per frame (row count matches fixture);
      `frame` integer, `timestamp`/`confidence` numeric, `success` 0/1.
- [ ] Column names are whitespace-trimmed: a header `" confidence"` yields
      column `confidence` (tested explicitly).
- [ ] AU columns parse as numeric; a spot AU intensity (`AU01_r`) and
      presence (`AU01_c`) equal the fixture's known values.
- [ ] `cli::cli_abort()` fires on: missing file; empty/garbage input.
- [ ] `devtools::test()` green; `devtools::check()` clean.

## Tasks

- [ ] Create `tests/testthat/fixtures/openface-sample.csv` — a small OpenFace
      output slice (few frames), space-padded headers, known values.
- [ ] Write failing tests (`tests/testthat/test-openface-read.R`): row count,
      column-name trimming, metadata types, AU values, error branches.
- [ ] Implement `of_read()` in `R/use_openface.R` (near `of_extract`,
      R/use_openface.R:53): read CSV, `trimws()` names, coerce to tibble;
      `cli::cli_abort()` on bad input.
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
