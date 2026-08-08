# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-08-07 (M08 merged and archived; M02 row pruned under terminal-row retention; harness-hardening candidate absorbed M08 review F6/F7)_
_Released 0.1.0 (GitHub) 2026-07-11._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M08 | GitHub Actions CI — R CMD check across platforms | done | — | high | milestones/archive/M08-github-actions-ci.md |
| M06 | Wrapper testing contract — system2-boundary command tests | done | — | high | milestones/archive/M06-wrapper-command-tests.md |
| M07 | Wrapper testing contract — remainder and gated real invocations | planned | M06 | normal | milestones/M07-wrapper-tests-remainder.md |
| M03 | whisper tidy reader (`aw_read`) | done | M01 | normal | milestones/archive/M03-whisper-reader.md |
| M04 | R CMD check hygiene (docs, namespace, build) | done | — | high | milestones/archive/M04-check-hygiene.md |
| M05 | Rewrite stale vignettes | done | M04 | normal | milestones/archive/M05-vignette-rewrite.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Validate `os_read` fixtures against a real openSMILE run (name quoting, `frameTime` in functionals output) — added 2026-07-11 — M01 review F5
- Evaluate wrapping modern ML tools (e.g., HuggingFace models) against the add-tool bar — added 2026-07-11 — DESIGN "Purpose & Scope"
- Drop/rework `os_fix_csv` on-disk normalization once `os_read` proves it redundant — added 2026-07-11 — M01
- Reader options deferred: `long=TRUE` pivot; OpenFace feature-block subsetting; whisper `$tokens` (separate reader) — added 2026-07-11 — M01–M03; RR01/D-008
- Let `aw_read` also accept a bare `$data` data.frame (convenience) — added 2026-07-11 — RR01 R6
- Multi-file reading idiom / `id`-column convention for readers (GP2 capability) — added 2026-07-11 — RR01 R7
- CRAN readiness: `audio.whisper` distribution decided (`Remotes:` must go; Additional_repositories vs. wrapping whisper.cpp — deferred to submission time; the two audio.whisper-expanding candidates above are on hold behind it) — added 2026-07-11 — design interview; the wrapper-testing-contract half became M06+M07 on 2026-08-07
- Platform-aware installer dispatchers (`install_openface()` detecting the OS and delegating), amending DESIGN's `_win`/`_mac` convention — needs a D-entry — added 2026-08-07 — M07 (guards only)
- Replace the hard-coded OneDrive model URLs with embedded authkeys in `install_openface_win` — added 2026-08-07 — DESIGN Known issues; M07
- Scout OpenFace successors (LibreFace / py-feat / OpenFace 3.0) against the add-tool bar — added 2026-07-11 — design interview
- Run-time tool-version capture (surface tool versions in outputs/attributes for methods reporting and drift debugging) — added 2026-07-11 — design interview
- Add the `test-coverage` / Codecov workflow (usethis `use_github_action("test-coverage")`); needs a `CODECOV_TOKEN` repository secret only the maintainer can add, so an unauthenticated job would block every later merge — added 2026-08-07 — M06 review 2; the check-standard half became M08 on 2026-08-07
- Turn on branch protection with the CI checks required on the default branch — GitHub repository settings, not files, so no milestone can land it — added 2026-08-07 — M08
- Harden the system2 test harness: redirect config in every test, assert absolute command paths, keep `args` unflattened, alias-safe `do.call` attribution, fail rather than skip when the coverage registry is empty; plus `fake_is_executable()`'s Windows branch resolving any existing path where real `Sys.which()` resolves by extension, and neither of its branches being covered in `test-helper-boundary.R` — added 2026-08-07 — M06 review 2 (R2, R13, R12, R3, R5); M08 review (F6, F7)
