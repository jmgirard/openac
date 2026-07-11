# M05: Rewrite stale vignettes

- **Status:** planned   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M04   <!-- check must be clean apart from vignettes first -->
- **Branch/PR:** —   <!-- m05-vignette-rewrite; PR URL once opened -->

## Goal

Fix the three stale vignettes so they use the current API and pass every
`R CMD check` vignette check, completing the "green up check" cleanup.

## Scope

**In:**
- Rewrite `vignettes/ffmpeg_wav.Rmd`, `vignettes/openface_parallel.Rmd`,
  `vignettes/opensmile_parallel.Rmd` to call current exported functions
  (`extract_opensmile`→`os_extract`, `extract_wav`→`os_prep_audio`/`ffmpeg`,
  `extract_openface`→`of_extract`, `extract_audio`→…).
- Set tool-calling chunks to `eval = FALSE` (external tools can't run during
  check/CI) while keeping the code shown correct and runnable by a user.
- Ensure vignettes knit and the check vignette sections pass.

**Out:**
- New vignettes or content beyond fixing these three → candidate.
- Making tool calls actually execute in CI (would require installing the
  external tools) → explicitly not done; chunks are `eval = FALSE`.

## Acceptance criteria

- [ ] None of the 3 vignettes reference removed function names (grep for
      `extract_opensmile|extract_wav|extract_openface|extract_audio` → 0).
- [ ] Tool-calling chunks are non-executing (`eval = FALSE`);
      `checking running R code from vignettes` → 0 errors.
- [ ] `checking package vignettes` and `checking files in 'vignettes'` →
      clean; `devtools::build_vignettes()` succeeds.
- [ ] `devtools::check()` (with M04 merged) → **0 errors, 0 warnings**; any
      remaining NOTEs explained.

## Tasks

- [ ] Map each stale function call to its current-API equivalent (read the
      `of_`/`os_`/`ffmpeg`/`aw_` families).
- [ ] Rewrite the 3 `.Rmd` files; set tool chunks `eval = FALSE`.
- [ ] `devtools::build_vignettes()`; confirm knit.
- [ ] `devtools::check()` — confirm full green (0E/0W).

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan (vignette half of the "green up R CMD
  check" cleanup; depends on M04).

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- filled by /milestone-review -->
