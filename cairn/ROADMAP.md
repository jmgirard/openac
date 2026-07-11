# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-11 (M04 merged)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M01 | openSMILE tidy reader (`os_read`) | done | — | high | [archive](milestones/archive/M01-opensmile-reader.md) |
| M02 | OpenFace tidy reader (`of_read`) | done | M01 | normal | [archive](milestones/archive/M02-openface-reader.md) |
| M03 | whisper tidy reader (`aw_read`) | review | M01 | normal | [M03](milestones/M03-whisper-reader.md) |
| M04 | R CMD check hygiene (docs, namespace, build) | done | — | high | [archive](milestones/archive/M04-check-hygiene.md) |
| M05 | Rewrite stale vignettes | planned | M04 | normal | [M05](milestones/M05-vignette-rewrite.md) |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Validate `os_read` fixtures against a real openSMILE run (name quoting, `frameTime` in functionals output) — added 2026-07-11 — M01 review F5
- Evaluate wrapping modern ML tools (e.g., HuggingFace models) against the add-tool bar — added 2026-07-11 — DESIGN "Purpose & Scope"
- Drop/rework `os_fix_csv` on-disk normalization once `os_read` proves it redundant — added 2026-07-11 — M01
- Reader options deferred: `long=TRUE` pivot; OpenFace feature-block subsetting; whisper `$tokens` (separate reader) — added 2026-07-11 — M01–M03; RR01/D-008
- Let `aw_read` also accept a bare `$data` data.frame (convenience) — added 2026-07-11 — RR01 R6
- Multi-file reading idiom / `id`-column convention for readers (GP2 capability) — added 2026-07-11 — RR01 R7
