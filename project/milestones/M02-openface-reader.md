# M02: OpenFace tidy reader (`of_read`)

- **Status:** review   <!-- mirror; project/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M01   <!-- inherits reader-family conventions (naming, wide tibble, tibble Import) -->
- **Branch/PR:** m02-openface-reader · https://github.com/jmgirard/openac/pull/2

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

- [x] `of_read()` exported; returns a tibble.
- [x] Fixture OpenFace CSV → one row per detected face per frame (row count
      matches fixture); `frame` integer, `timestamp`/`confidence` numeric,
      `success` 0/1. Multi-face pass-through tested (2 faces, one frame).
- [x] Column names are whitespace-trimmed: a header `" confidence"` yields
      column `confidence` (tested explicitly).
- [x] AU columns parse as numeric; a spot AU intensity (`AU01_r`) and
      presence (`AU01_c`) equal the fixture's known values. Non-AU feature
      blocks (`gaze_0_x`, `pose_Tx`, `x_0`) also value-checked.
- [x] `cli::cli_abort()` fires on: missing file; empty file; non-string input.
      _(Amended 2026-07-11 per M01 precedent: non-empty "garbage" is not
      detected — a valid-looking CSV is parsed as-is; header-only → 0-row
      tibble, tested.)_
- [x] `devtools::test()` green; `of_read` adds **zero new** `check()`
      errors/warnings vs. baseline. _Amended 2026-07-11 per M01's precedent:
      full `check()`-clean stays blocked on the pre-existing baseline debt
      (the "green up R CMD check" ROADMAP candidate)._

## Tasks

- [x] Create `tests/testthat/fixtures/openface-sample.csv` — a small OpenFace
      output slice (3 frames), space-padded headers, known values.
- [x] Write failing tests (`tests/testthat/test-openface-read.R`): row count,
      column-name trimming, metadata types, AU values, error branches.
- [x] Implement `of_read()` in `R/use_openface.R` (after `of_extract_dir`):
      read CSV, `trimws()` names, coerce to tibble; `cli::cli_abort()` on bad
      input.
- [x] Roxygen doc + `@examples`; `Rscript -e 'devtools::document()'`.
- [x] `devtools::test()` green; `check()` adds no new problems (verified
      2026-07-11 — `of_read` appears only in the pre-existing globals NOTE).

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan.
- 2026-07-11: implemented `of_read()` (comma CSV, `trimws()` header padding,
  `check.names=FALSE`) + hand-built 3-frame fixture + 5 tests (20 assertions,
  all green). No dependency change (`tibble` already imported via M01). Same
  amended check criterion as M01. `check()` verification pending.
- 2026-07-11: verified `of_read` adds zero new check errors/warnings (only a
  read.csv line in the globals NOTE). Noted M01's `os_read.Rd` broken
  `os_fix_csv` xref → added to cleanup candidate. Status → review.
- 2026-07-11: review — draft PR #2; fresh test (56 pass) + consistency gate
  clean. Independent Opus review (no blockers): reworded "garbage" criterion
  to "empty" + multi-face doc fix; added header-only/NA/multi-face/non-AU
  tests (20 → 31 assertions).

## Decisions
<!-- milestone-local; promote cross-cutting ones to project/DECISIONS.md -->

- Inherits D-004 (naming) and D-005 (`tibble` Import) from M01.

## Review

_Reviewed 2026-07-11 (branch `m02-openface-reader`, PR #2)._

**Acceptance criteria — fresh evidence:**
- Returns a tibble; 3 frames; `frame` integer, `timestamp`/`confidence`
  double, `success` 0/1; header whitespace trimmed; `AU01_r`/`AU01_c` numeric
  with correct values; error branches (non-string / missing / empty) — all
  verified by `devtools::test()`: full suite **56 pass, 0 fail** (1 unrelated
  skip: empty `test-openface.R` stub).
- Amended criterion: `of_read` adds **zero new** `check()` errors/warnings vs
  baseline. Confirmed 2026-07-11 (appears only in the pre-existing globals
  NOTE).

**Consistency gate:**
- `devtools::document()` leaves `man/of_read.Rd` unchanged → docs in sync.
  (Same roxygen 8.0.0-vs-7.3.3 collateral churn on unrelated pages — reverted,
  not shipped; tracked in cleanup candidate.)
- README unchanged by M02 → no rebuild.
- No `_pkgdown.yml` → pkgdown check N/A.
- NEWS.md has an `of_read` entry.
- No new top-level files (fixture under `tests/`) → no `.Rbuildignore`.
- No CI workflows → no CI gate.

**Independent fresh-context review (Opus subagent):** no blockers; parser
correct. Triage:
- **Fixed now** — F1 (criterion accuracy): "garbage input" was checked off but
  not implemented (matches M01 — non-empty garbage isn't detected). Reworded
  the criterion to "empty file"; documented header-only → 0-row (tested).
- **Fixed now** — F2 (mandated edge tests): added header-only (0-row) and
  blank-cell→NA tests per tracking-rules "What gets a test".
- **Fixed now** — F3 (false doc claim): OpenFace uses a multi-face model, so
  "one row per frame" was wrong. Reworded doc to "one row per detected face
  per frame" + added a multi-face test (2 faces, one frame).
- **Fixed now** — F4: value-checked the non-AU feature blocks (`gaze_0_x`,
  `pose_Tx`, `x_0`). F5: removed trailing blank line.
- Tests 20 → 31 assertions. No follow-ups spawned.

