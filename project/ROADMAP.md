# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-11_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M01 | openSMILE tidy reader (`os_read`) | done | — | high | [archive](milestones/archive/M01-opensmile-reader.md) |
| M02 | OpenFace tidy reader (`of_read`) | in-progress | M01 | normal | [M02](milestones/M02-openface-reader.md) |
| M03 | whisper tidy reader (`aw_read`) | planned | M01 | normal | [M03](milestones/M03-whisper-reader.md) |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Green up `R CMD check`: fix 3 stale vignettes (`extract_opensmile`→`os_extract` etc.), document `install_opensmile_{mac,win}`/`os_check_config`, add utils/stats importFroms, `.Rbuildignore` `.claude`, resolve roxygen pin (project 7.3.3 vs installed 8.0.0), fix `os_read.Rd` broken `\link{os_fix_csv}` xref, drop unused `pak` Import / declare `purrr` — added 2026-07-11 — surfaced by M01/M02
- Validate `os_read` fixtures against a real openSMILE run (name quoting, `frameTime` in functionals output) — added 2026-07-11 — M01 review F5
- Evaluate wrapping modern ML tools (e.g., HuggingFace models) against the add-tool bar — added 2026-07-11 — DESIGN "Purpose & Scope"
- Drop/rework `os_fix_csv` on-disk normalization once `os_read` proves it redundant — added 2026-07-11 — M01
- Reader options deferred: `long=TRUE` pivot; OpenFace feature-block subsetting; whisper `$tokens`/speaker columns — added 2026-07-11 — M01–M03
