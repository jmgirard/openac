# M04: R CMD check hygiene (docs, namespace, build) — done

**Status:** done · **PR:** #3 · merged 2026-07-11 · Priority: high

## Goal
Clear every non-vignette `R CMD check` ERROR/WARNING/NOTE so main checks
cleanly apart from the vignettes (M05's scope).

## Outcome
Baseline 1E / 5W / 3N → `check()` (vignettes off) **0E / 0W / 0N** apart from
the 2 named vignette warnings (deferred to M05). `test()` 67 pass.
- 9 tool-dependent `@examples` → `\dontrun{}`; documented
  `install_opensmile_mac`/`_win`, `os_check_config`.
- Rd xrefs: `whisper`/`audio.whisper`/`os_fix_csv` links → `\code{}` text;
  `aw_get_model` `@inheritParams`/`@seealso` reworked.
- DESCRIPTION `+purrr −pak`; `pwalk` site commented; new
  `R/openac-package.R` with `@importFrom stats/utils`.
- `.Rbuildignore` `^\.claude$`; `NEWS.md` parseable header; `RoxygenNote`
  → 8.0.0; empty `test-openface.R` removed.

## Key decisions
- D-006 (purrr in / pak out; whisper sequential-by-design).
- Gate: audio.whisper left uninstalled — `\code{}` xref fix makes check pass
  regardless; criterion refined to "verified without it".
