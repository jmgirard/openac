# M01: openSMILE tidy reader (`os_read`)

- **Status:** review   <!-- mirror; project/ROADMAP.md is the authority -->
- **Priority:** high   <!-- high | normal | low -->
- **Depends on:** —
- **Branch/PR:** m01-opensmile-reader · https://github.com/jmgirard/openac/pull/1

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
- [x] `devtools::test()` green; `os_read` adds **zero new** `check()`
      errors/warnings vs. the `main` baseline (verified 2026-07-11 — the only
      `os_read` line in output is one entry in a pre-existing globals NOTE,
      matching `os_fix_csv`). _Amended 2026-07-11: full `check()`-clean is
      blocked on pre-existing baseline debt out of M01's scope; tracked as a
      separate cleanup milestone candidate (see ROADMAP)._

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
- 2026-07-11: `check()` red on **pre-existing baseline debt**, not `os_read`:
  audio.whisper Suggests missing (env); 3 stale vignettes calling renamed
  fns; undocumented `install_opensmile_{mac,win}`/`os_check_config`;
  `.claude` not Rbuildignored; base-fn globals note. `os_read` adds 0 new
  errors/warnings. "check clean" criterion blocked on baseline — awaiting
  user decision on scope.
- 2026-07-11: user chose to keep M01 focused (amend criterion to "no new
  check errors/warnings vs baseline") and track baseline debt as a separate
  cleanup milestone candidate. Status → review.
- 2026-07-11: review — draft PR #1; fresh test (24 pass) + consistency gate
  clean (os_read docs in sync; NEWS entry added). Independent Opus review:
  fixed apostrophe-name parsing bug (F1) + added NaN/Inf/NA + header-only
  tests (F2/F3); tests now 36 pass. F5 → fixture-fidelity candidate.

## Decisions
<!-- milestone-local; promote cross-cutting ones to project/DECISIONS.md -->

- Reader-family naming (`<tool>_read`) → D-004; `tibble` Import → D-005 (both
  cross-cutting, recorded in project/DECISIONS.md).

## Review

_Reviewed 2026-07-11 (branch `m01-opensmile-reader`, PR #1)._

**Acceptance criteria — fresh evidence:**
- Returns a tibble; agg (1 row, `name`, numeric features, spot values); LLD
  (4 rows, `frameTime` numeric & non-decreasing); delimiter parity; error
  branches (non-string / missing / empty) — all verified by
  `devtools::test()`: **24 pass, 0 fail** (1 unrelated skip: empty
  `test-openface.R` stub).
- `tibble` present in DESCRIPTION Imports (D-005). Confirmed.
- Amended criterion: `os_read` adds **zero new** `check()` errors/warnings vs
  baseline. Confirmed 2026-07-11 (the sole `os_read` line in `check()` output
  is one entry in a pre-existing globals NOTE, mirroring `os_fix_csv`).

**Consistency gate:**
- `devtools::document()` leaves `man/os_read.Rd` unchanged → os_read docs in
  sync. (It does churn DESCRIPTION + 7 unrelated man pages via the roxygen
  8.0.0-vs-pinned-7.3.3 mismatch — pre-existing env debt, tracked in the
  cleanup candidate; reverted, not shipped.)
- README.Rmd/README.md present; M01 does not change README content → no
  rebuild needed.
- No `_pkgdown.yml` → pkgdown check N/A.
- NEWS.md created with an `os_read` entry (no NEWS.md existed before).
- No new top-level files needing `.Rbuildignore` (fixtures live under
  `tests/`; NEWS.md is a standard build file).
- No CI workflows in repo → no CI gate.

**Independent fresh-context review (Opus subagent):** no blockers; criteria
met. Triage of findings:
- **Fixed now** — F1 (correctness): `quote = "\"'"` made an unquoted instance
  name containing an apostrophe (e.g. `Bob's interview.wav`) silently swallow
  the file (0 rows). Changed to double-quote-only + regression test.
- **Fixed now** — F2/F3 (test gaps): added coverage for `NaN`/`Inf`/`NA`
  feature values and a header-only (0-row) file. (Test count 24 → 36.)
- **Accepted, no change** — F4 (lexical delimiter sniff on line 1) and F6
  (parity test is a re-read by design; independent oracles are the agg/LLD
  value tests). Documented as known limitations.
- **Follow-up candidate** — F5: hand-built fixtures don't fully mirror real
  openSMILE output (name quoting, `frameTime` in functionals). Validate
  against a real run → ROADMAP candidate.

