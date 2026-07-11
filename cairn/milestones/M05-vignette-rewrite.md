# M05: Rewrite stale vignettes

- **Status:** review   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M04   <!-- check must be clean apart from vignettes first -->
- **Branch/PR:** m05-vignette-rewrite   <!-- PR URL once opened -->

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

- [x] None of the 3 vignettes reference removed function names (grep for
      `extract_opensmile|extract_wav|extract_openface|extract_audio` → 0).
- [x] Tool-calling chunks are non-executing (`eval = FALSE` set globally per
      vignette); `checking running R code from vignettes` → 0 errors.
- [x] `checking package vignettes` and `checking files in 'vignettes'` →
      clean; vignettes knit (`tools::buildVignettes()` — see amendment).
- [x] `devtools::check()` (with M04 merged) → **0 errors, 0 warnings, 0
      notes** — full green, vignettes built.

## Tasks

- [x] Map each stale function call to its current-API equivalent:
      `extract_wav`→`os_prep_audio`, `extract_openface`→`of_extract`,
      `extract_opensmile`→`os_extract` (signatures verified against R/).
- [x] Rewrite the 3 `.Rmd` files; set `eval = FALSE` globally; also point to
      the `_dir` batch convenience functions and the `*_read()` readers.
- [x] Confirm knit via `tools::buildVignettes()` (all 3 re-build cleanly).
- [x] `devtools::check()` — full green: 0E / 0W / 0N.

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan (vignette half of the "green up R CMD
  check" cleanup; depends on M04).
- 2026-07-11: started; branch m05-vignette-rewrite cut from main.
- 2026-07-11: amendment (minor) — used `tools::buildVignettes()` (base R) to
  confirm knitting instead of `devtools::build_vignettes()`, which is
  deprecated and requires the uninstalled `remotes` package. The authoritative
  gate is the full `devtools::check()` anyway.
- 2026-07-11: amendment (minor) — set `eval = FALSE` for the whole workflow
  (not just tool chunks): the file-finding/path chunks reference absent data
  and feed the tool chunks, so a coherent illustrative vignette runs nothing
  past `library()`. Also added `_dir` batch functions + `*_read()` readers as
  a natural (not "new content") pointer.
- 2026-07-11: rewrote all 3 vignettes to the current API; full `check()` now
  0E/0W/0N (the M04+M05 green-up arc is complete). Status → review.

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- filled by /milestone-review -->
