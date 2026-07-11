# M04: R CMD check hygiene (docs, namespace, build, examples)

- **Status:** in-progress   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- high | normal | low -->
- **Depends on:** —
- **Branch/PR:** m04-check-hygiene   <!-- PR URL once opened -->

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

- [ ] `checking examples` → 0 errors (all tool-dependent examples `\dontrun`).
- [ ] `checking for missing documentation entries` → clean (the 3 objects
      documented).
- [ ] `checking Rd cross-references` → clean (no missing links / unknown
      package xrefs, incl. `os_read.Rd`, `aw_*.Rd`).
- [ ] `checking dependencies in R code` → clean (`purrr` declared, `pak`
      removed; D-006 recorded).
- [ ] `checking R code for possible problems` → no undefined-globals note
      (`utils`/`stats` importFroms present).
- [ ] `checking for hidden files` and `checking package subdirectories` →
      clean (`.claude` in `.Rbuildignore`; `NEWS.md` parses).
- [ ] `devtools::document()` produces no diff (RoxygenNote 8.0.0); empty
      `test-openface.R` removed; `devtools::test()` green.
- [ ] `devtools::check()` (with `audio.whisper` Suggests available) → 0
      errors, 0 warnings **except** the vignette checks, which are named and
      deferred to M05.

## Tasks

- [ ] Audit the 11 `@examples`; wrap tool/binary-dependent ones in
      `\dontrun{}`; `devtools::document()`.
- [ ] Write roxygen for `install_opensmile_mac`, `install_opensmile_win`,
      `os_check_config`.
- [ ] Fix Rd xrefs in `os_read.Rd` (roxygen in `use_opensmile.R`) and
      `aw_*` (roxygen in `use_whisper.R`).
- [ ] DESCRIPTION: add `purrr`, remove `pak`; comment the `pwalk` site;
      append D-006 to DECISIONS.md.
- [ ] Add `utils`/`stats` importFroms (roxygen `@importFrom`).
- [ ] `.Rbuildignore` `^\.claude$`; fix `NEWS.md` header so news parses.
- [ ] Bump `RoxygenNote` to 8.0.0; `devtools::document()` (regenerate man/).
- [ ] Remove empty `tests/testthat/test-openface.R`; `devtools::test()`.
- [ ] `devtools::check()` (audio.whisper installed) — confirm criteria.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan (promoted from the "green up R CMD
  check" candidate; split hygiene here, vignettes → M05).
- 2026-07-11: started; branch m04-check-hygiene cut from main.

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

- Dependency change (add `purrr`, remove `pak`) + whisper sequential-by-design
  → D-006 (cross-cutting; in cairn/DECISIONS.md).

## Review
<!-- filled by /milestone-review -->
