# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-08-07 (re-audit after the PR #9 hotfix — all checks green, nothing in flight, no untriaged issues or PRs; no changes needed)_
_Released 0.1.0 (GitHub) 2026-07-11._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M09 | Test-harness hardening — fake fidelity and a non-vacuous coverage gate | review | — | normal | milestones/M09-harness-hardening.md |
| M08 | GitHub Actions CI — R CMD check across platforms | done | — | high | milestones/archive/M08-github-actions-ci.md |
| M06 | Wrapper testing contract — system2-boundary command tests | done | — | high | milestones/archive/M06-wrapper-command-tests.md |
| M07 | Wrapper testing contract — remainder and gated real invocations | done | M06 | normal | milestones/archive/M07-wrapper-tests-remainder.md |
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
- A working macOS OpenFace installer (`install_openface_mac()` was an inert stub and was deleted; the Homebrew/cmake build it sketched is the starting point) — added 2026-08-07 — M07
- Platform-aware installer dispatchers (`install_openface()` detecting the OS and delegating), amending DESIGN's `_win`/`_mac` convention — needs a D-entry — added 2026-08-07 — M07 (guards only)
- Replace the hard-coded OneDrive model URLs with embedded authkeys in `install_openface_win` — added 2026-08-07 — DESIGN Known issues; M07
- Scout OpenFace successors (LibreFace / py-feat / OpenFace 3.0) against the add-tool bar — added 2026-07-11 — design interview
- Run-time tool-version capture (surface tool versions in outputs/attributes for methods reporting and drift debugging) — added 2026-07-11 — design interview
- Add the `test-coverage` / Codecov workflow (usethis `use_github_action("test-coverage")`); needs a `CODECOV_TOKEN` repository secret only the maintainer can add, so an unauthenticated job would block every later merge — added 2026-08-07 — M06 review 2; the check-standard half became M08 on 2026-08-07
- Turn on branch protection with the CI checks required on the default branch — GitHub repository settings, not files, so no milestone can land it — added 2026-08-07 — M08
- Restore GP6 for output-path collisions — drop colliding files into the `*_dir` outcome table as per-file failures instead of aborting the batch pre-flight; needs plumbing the derivation result through `dir_walk` — added 2026-08-07 — hotfix batch-extension-case, PR #9
- `R CMD check` carries a standing spelling NOTE — 55 domain terms (openSMILE, ffmpeg, wav, PCM, ORCID…) flagged against a 2-entry `inst/WORDLIST`; run `spelling::update_wordlist()` and keep it current, or drop `tests/spelling.R` — added 2026-08-07 — hotfix batch-extension-case, PR #9
