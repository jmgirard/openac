# M04: R CMD check hygiene (docs, namespace, build, examples)

- **Status:** review   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- high | normal | low -->
- **Depends on:** —
- **Branch/PR:** m04-check-hygiene · https://github.com/jmgirard/openac/pull/3

## Goal

Clear every non-vignette `R CMD check` ERROR/WARNING/NOTE so `main` checks
cleanly apart from the vignettes (which M05 handles).

## Scope

**In:**
- Wrap all external-tool/binary-dependent `@examples` in `\dontrun{}` (11
  `@examples` across `programs_check`, `use_ffmpeg`, `use_ffprobe`,
  `use_opensmile`, `use_openface`) so `checking examples` passes.
- Document the three undocumented exports: `install_opensmile_mac`,
  `install_opensmile_win`, `os_check_config` (title/params/return/`\dontrun`).
- Fix Rd cross-references: remove `os_read.Rd`'s broken `\link{os_fix_csv}`
  (internal fn); make `aw_transcribe.Rd`/`aw_transcribe_dir.Rd` `whisper` and
  `audio.whisper` references not require an uninstalled package (use
  `\code{}` text, not `\link`).
- Dependency change (D-006): add `purrr` to Imports (used at
  `use_whisper.R:488`, deliberately sequential), remove unused `pak`; add a
  comment at the `pwalk` site explaining the sequential-by-design rationale.
- Add `@importFrom utils read.csv write.csv` and `@importFrom stats predict`
  to clear the undefined-globals NOTE.
- Add `^\.claude$` to `.Rbuildignore` (hidden-files NOTE); make `NEWS.md`
  parse under R's news reader (subdirectories NOTE "No news entries found").
- Bump `RoxygenNote` to 8.0.0 and regenerate all man pages once (ends the
  collateral-churn friction).
- Remove the empty `tests/testthat/test-openface.R` stub.

**Out:**
- Rewriting the 3 stale vignettes → M05 (depends on this).
- Installing external tools (ffmpeg/openface/opensmile) — unneeded once
  examples are `\dontrun`.
- Exporting/keeping `os_fix_csv` behaviour changes → separate candidate.

## Acceptance criteria

- [x] `checking examples` → 0 errors (all tool-dependent examples `\dontrun`).
- [x] `checking for missing documentation entries` → clean (the 3 objects
      documented).
- [x] `checking Rd cross-references` → clean (no missing links / unknown
      package xrefs, incl. `os_read.Rd`, `aw_*.Rd`).
- [x] `checking dependencies in R code` → clean (`purrr` declared, `pak`
      removed; D-006 recorded).
- [x] `checking R code for possible problems` → no undefined-globals note
      (`utils`/`stats` importFroms present).
- [x] `checking for hidden files` and `checking package subdirectories` →
      clean (`.claude` in `.Rbuildignore`; `NEWS.md` parses).
- [x] `devtools::document()` produces no diff (RoxygenNote 8.0.0); empty
      `test-openface.R` removed; `devtools::test()` green.
- [x] `devtools::check()` → 0 errors, 0 warnings **except** the vignette
      checks, which are named and deferred to M05. (Verified *without*
      `audio.whisper` installed; the `\code{}` xref fix below makes its
      presence irrelevant — see 2026-07-11 amendment.)

## Tasks

- [x] Audit the 11 `@examples`; wrap tool/binary-dependent ones in
      `\dontrun{}` (9 wrapped); `devtools::document()`.
- [x] Write roxygen for `install_opensmile_mac`, `install_opensmile_win`,
      `os_check_config`.
- [x] Fix Rd xrefs in `os_read.Rd` (roxygen in `use_opensmile.R`) and
      `aw_*` (roxygen in `use_whisper.R`). Also `aw_get_model` `@inheritParams`
      /`@seealso` → `\code{}` text (discovered: 8.0.0 regen without
      audio.whisper stripped its `\arguments{}`).
- [x] DESCRIPTION: add `purrr`, remove `pak`; comment the `pwalk` site
      (D-006 already in DECISIONS.md).
- [x] Add `utils`/`stats` importFroms (new `R/openac-package.R` `@importFrom`).
- [x] `.Rbuildignore` `^\.claude$`; fix `NEWS.md` header so news parses.
- [x] Bump `RoxygenNote` to 8.0.0; `devtools::document()` (regenerate man/).
- [x] Remove empty `tests/testthat/test-openface.R`; `devtools::test()`.
- [x] `devtools::check()` (vignettes off) — confirm criteria: 0E / 0W / 0N
      apart from the 2 deferred vignette warnings.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan (promoted from the "green up R CMD
  check" candidate; split hygiene here, vignettes → M05).
- 2026-07-11: started; branch m04-check-hygiene cut from main.
- 2026-07-11: baseline `check()` (vignettes off) = 1E/5W/3N, matches plan.
- 2026-07-11: amendment (gate) — dropped "with audio.whisper available" from
  the final criterion. The `\link{whisper}`→`\code{}` fix makes check pass
  regardless, and audio.whisper (Remotes, whisper.cpp compile) isn't
  installed; verifying without it. User asked for a recommendation; took it.
- 2026-07-11: amendment (minor) — reordered tasks so the RoxygenNote 8.0.0
  bump + full man/ regen runs first, isolating version churn from the
  content edits.
- 2026-07-11: all tasks done. `test()` 67 pass; `check()` (vignettes off) =
  0E/0W/0N except the 2 named vignette warnings (M05). Status → review.
- 2026-07-11: review — PR #3 opened; fresh evidence re-gathered; independent
  Opus review clean (1 nit fixed: `install_opensmile_win` `@return` wording).

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

- Dependency change (add `purrr`, remove `pak`) + whisper sequential-by-design
  → D-006 (cross-cutting; in cairn/DECISIONS.md).

## Review

_Reviewed 2026-07-11 · PR #3 · branch `m04-check-hygiene`._

**Acceptance criteria (fresh evidence):**
- Examples 0 errors ✓; missing-docs clean ✓; Rd xrefs clean ✓; deps clean
  (`purrr` in, `pak` out) ✓; possible-problems no undefined globals ✓;
  hidden-files + subdirectories clean ✓; `document()` no diff ✓,
  `test-openface.R` removed ✓, `test()` 67 pass ✓.
- `check()` (vignettes off) = 0E / 0W / 0N apart from the 2 named vignette
  warnings (M05). Full-vignette check still errors — that is M05's scope.

**Consistency gate:** `document()` idempotent ✓; README.Rmd/.md untouched &
in sync ✓; no pkgdown site (no `_pkgdown.yml` row owed) ✓; no NEWS entry
(hygiene only, no user-visible behavior change) ✓; no new top-level tracked
files (`.claude` now ignored) ✓.

**Independent Opus review (fresh context):** no blockers, no regressions,
no out-of-scope changes. One NIT — `install_opensmile_win` `@return` wrongly
said "(invisibly returned)"; fixed to match the macOS twin. No CI configured
(no `.github/workflows`), so no remote checks to gate on.
