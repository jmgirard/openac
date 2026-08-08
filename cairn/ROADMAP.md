# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-08-08 (M12 at review; the tidymedia candidate retired into it and the spelling-NOTE candidate retired by a direct wordlist commit; three rows added from M12's assessment; five terminal rows, at retention)_
_Released 0.1.0 (GitHub) 2026-07-11._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M12 | Does openac belong on top of tidymedia? — a fit assessment, and a decision | review | — | high | milestones/M12-tidymedia-fit-assessment.md |
| M11 | A wholly-skipped test file cannot exist — the coverage gate's blind spot, closed at the door | done | M10 | normal | milestones/archive/M11-forbid-top-level-skips.md |
| M09 | Test-harness hardening — fake fidelity at the tool boundary | done | — | normal | milestones/archive/M09-harness-hardening.md |
| M10 | Command-contract coverage gate — completeness observed, not inferred | done | M09 | high | milestones/archive/M10-coverage-gate-run-scope.md |
| M08 | GitHub Actions CI — R CMD check across platforms | done | — | high | milestones/archive/M08-github-actions-ci.md |
| M07 | Wrapper testing contract — remainder and gated real invocations | done | M06 | normal | milestones/archive/M07-wrapper-tests-remainder.md |

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
- Quote at the process boundary, not at the call site — adopt a token-vector + shQuote contract in the passthroughs (the pattern tidymedia centralizes in run_program) so a forgotten literal quote at one call site cannot ship a broken command; today every caller hand-quotes paths (R/use_ffprobe.R:51-56) — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, E7)
- Make ffp_count_streams resilient rather than fatal — it aborts on an unreadable file (R/use_ffprobe.R:48) where a batch would rather get NA and a warning, as tidymedia's probe_all does (R/ffprobe.R:119-124); relates to GP6/GP9 — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, E5)
- Document that openac and tidymedia share 8 exported names of which 6 disagree (ffm most sharply: passthrough alias vs. job constructor), so attaching both masks silently — a README/vignette note, or a rename at the 1.0 API freeze — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, C1-C8)
- Restore GP6 for output-path collisions — drop colliding files into the `*_dir` outcome table as per-file failures instead of aborting the batch pre-flight; needs plumbing the derivation result through `dir_walk` — added 2026-08-07 — hotfix batch-extension-case, PR #9
