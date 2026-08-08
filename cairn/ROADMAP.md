# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-08-08 (M15 and M16 planned for the Windows host; the Windows `%` candidate graduated into M15; M14, M15 and M16 are all workable)_
_Released 0.1.0 (GitHub) 2026-07-11._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M13 | Quote at the process boundary, not at the call site | done | — | high | milestones/archive/M13-boundary-quoting.md |
| M14 | A bad file is an outcome, not the end of the batch | planned | M13 | normal | milestones/M14-resilient-stream-count.md |
| M15 | What Windows actually does to a path the shell can eat | planned | — | high | milestones/M15-windows-quoting-oracle.md |
| M16 | The Windows installers, actually run | planned | — | normal | milestones/M16-windows-installers-real-run.md |
| M12 | Does openac belong on top of tidymedia? — a fit assessment, and a decision | done | — | high | milestones/archive/M12-tidymedia-fit-assessment.md |
| M11 | A wholly-skipped test file cannot exist — the coverage gate's blind spot, closed at the door | done | M10 | normal | milestones/archive/M11-forbid-top-level-skips.md |
| M09 | Test-harness hardening — fake fidelity at the tool boundary | done | — | normal | milestones/archive/M09-harness-hardening.md |
| M10 | Command-contract coverage gate — completeness observed, not inferred | done | M09 | high | milestones/archive/M10-coverage-gate-run-scope.md |

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
- Replace the hard-coded OneDrive model URLs with embedded authkeys in `install_openface_win` — added 2026-08-07 — DESIGN Known issues; M07; promoted only if M16's real run measures a link dead
- Scout OpenFace successors (LibreFace / py-feat / OpenFace 3.0) against the add-tool bar — added 2026-07-11 — design interview
- Run-time tool-version capture (surface tool versions in outputs/attributes for methods reporting and drift debugging) — added 2026-07-11 — design interview
- Add the `test-coverage` / Codecov workflow (usethis `use_github_action("test-coverage")`); needs a `CODECOV_TOKEN` repository secret only the maintainer can add, so an unauthenticated job would block every later merge — added 2026-08-07 — M06 review 2; the check-standard half became M08 on 2026-08-07
- Turn on branch protection with the CI checks required on the default branch — GitHub repository settings, not files, so no milestone can land it — added 2026-08-07 — M08
- Show the constructed command — GP5's remaining half; once M13 makes commands token vectors, a display/return surface is cheap (tidymedia renders both from one structure via ffm_compile/ffm_args, R/ffm.R:1152,1164) — added 2026-08-08 — M13 Out; D-017
- Run `install_opensmile_mac()` for real on macOS — the mac half of the gap M16 closes on Windows; the installers have only ever been exercised against a mocked download environment — added 2026-08-08 — M16 Out
- Extend M14's per-file-outcome treatment to the other probe-and-abort guards (os_check_config, the stopifnot(file.exists()) guards in of_extract/os_extract) — added 2026-08-08 — M14 Out
- Revisit the 8 names openac and tidymedia both export at the 1.0 API freeze — the README now warns users (2026-08-08); renaming is the other half and only makes sense once the API is frozen — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, C1-C8)
- Restore GP6 for output-path collisions — drop colliding files into the `*_dir` outcome table as per-file failures instead of aborting the batch pre-flight; needs plumbing the derivation result through `dir_walk` — added 2026-08-07 — hotfix batch-extension-case, PR #9
