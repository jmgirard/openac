# M05: Rewrite stale vignettes — done

**Status:** done · **PR:** #5 · merged 2026-07-11 · Priority: normal · Depends: M04

## Goal
Fix the three stale vignettes (which called removed functions) so they use the
current API and pass every `R CMD check` vignette check — completing the
"green up check" cleanup begun in M04.

## Outcome
Rewrote `ffmpeg_wav`, `openface_parallel`, `opensmile_parallel` to current
exports: `extract_wav`→`os_prep_audio()`, `extract_openface`→`of_extract()`,
`extract_opensmile`→`os_extract()`. Kept the `future_*` parallel structure,
set `eval = FALSE` globally (workflows drive external tools over user data),
and added the `_dir` batch functions and `*_read()` readers as pointers.
Removed function names gone; vignettes knit. **Full `devtools::check()` now
0E/0W/0N** — the package passes R CMD check fully clean for the first time.

## Key decisions
- Minor amendments (logged): used base-R `tools::buildVignettes()` to confirm
  knitting (`devtools::build_vignettes()` is deprecated + needs `remotes`);
  set `eval = FALSE` for whole workflows, not just tool chunks.
- Independent Opus review: APPROVE, clean (every call verified vs real
  signatures).
