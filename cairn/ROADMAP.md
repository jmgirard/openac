# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-08-09 (M18 merged and archived after two review rounds — the first caught the new skip signal cancelling the work `overwrite = FALSE` was meant to preserve, plus a `#'` leaked into five rendered `@return` blocks; the second, a self-contradicting man page and three stale vignettes. D-019 recorded; 2 candidates, 2 lessons; 2 lessons and the M13 row pruned at their caps)_
_Released 0.1.0 (GitHub) 2026-07-11._

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M17 | A tool that exited non-zero is a failed file | done | — | high | milestones/archive/M17-tool-exit-status.md |
| M18 | A skipped file is a skip, not a success | done | M17 | normal | milestones/archive/M18-batch-skip-outcome.md |
| M19 | A guard that names no file | planned | — | normal | milestones/M19-guards-name-the-file.md |
| M14 | A bad file is an outcome, not the end of the batch | done | M13 | normal | milestones/archive/M14-resilient-stream-count.md |
| M15 | What Windows actually does to a path the shell can eat | done | — | high | milestones/archive/M15-windows-quoting-oracle.md |
| M16 | The Windows installers, actually run | done | — | normal | milestones/archive/M16-windows-installers-real-run.md |

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
- Scout OpenFace successors (LibreFace / py-feat / OpenFace 3.0) against the add-tool bar — added 2026-07-11 — design interview
- Run-time tool-version capture (surface tool versions in outputs/attributes for methods reporting and drift debugging) — added 2026-07-11 — design interview
- Add the `test-coverage` / Codecov workflow (usethis `use_github_action("test-coverage")`); needs a `CODECOV_TOKEN` repository secret only the maintainer can add, so an unauthenticated job would block every later merge — added 2026-08-07 — M06 review 2; the check-standard half became M08 on 2026-08-07
- Turn on branch protection with the CI checks required on the default branch — GitHub repository settings, not files, so no milestone can land it — added 2026-08-07 — M08
- Show the constructed command — GP5's remaining half; once M13 makes commands token vectors, a display/return surface is cheap (tidymedia renders both from one structure via ffm_compile/ffm_args, R/ffm.R:1152,1164) — added 2026-08-08 — M13 Out; D-017
- Host the four OpenFace patch experts somewhere openac controls — M16 measured the OneDrive links dead (200 + sign-in page) and repointed to OpenFace's Dropbox links, which are the same shape of hazard on someone else's account; a release asset on this repo would end the class — added 2026-08-08 — M16 Review
- Pin `install_ffmpeg_win()` to a version — its gyan.dev URL is a `-release-essentials` alias that resolved to 9.0 on 2026-08-08, so the installed ffmpeg moves with upstream and no two installs are guaranteed alike — added 2026-08-08 — M16 Review; DESIGN Known issues
- OpenFace writes no `.csv` on Windows for a face-less input — it tracks to completion and writes `faces_of_details.txt` beside the requested `faces.csv`, where the same input on macOS yields a header-only CSV; `test-real-tools.R`'s OpenFace test fails on Windows on `main` as well as on the branch, so `devtools::check()` errors there — added 2026-08-08 — M15 review, out of scope
- Two installer-guard robustness fixes M16's review logged just under the action bar: `download_model()`'s `if (size < floor)` errors instead of returning `FALSE` when `file.size()` gives `NA` (a Windows stat race), and `skip_if_not_installed("curl")` sits in the shared gate so a maintainer without curl loses all four opt-in tests rather than the one probe using it — added 2026-08-08 — M16 Review N10, N13
- Run `install_opensmile_mac()` for real on macOS — the mac half of the gap M16 closes on Windows; the installers have only ever been exercised against a mocked download environment — added 2026-08-08 — M16 Out
- Revisit the 8 names openac and tidymedia both export at the 1.0 API freeze — the README now warns users (2026-08-08); renaming is the other half and only makes sense once the API is frozen — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, C1-C8)
- The second ffprobe query is unchecked in `os_check_audio()` (R/use_opensmile.R:139) and `aw_check_audio()` (R/use_whisper.R:41) — `ffp_count_streams()` guards only the FIRST probe, so a non-zero exit on the codec/rate/channel query surfaces as `subscript out of bounds` in the openSMILE one (no length guard) and as a silent `FALSE` in the whisper one (which has one), i.e. "not ready" rather than "unreadable" — added 2026-08-09 — M17 Review, finding I (63); M17 Out excluded ffprobe deliberately
- Restore GP6 for output-path collisions — drop colliding files into the `*_dir` outcome table as per-file failures instead of aborting the batch pre-flight; waits on M18, whose skip channel is the pre-marked-row plumbing this needs (`dir_outputs()` returns a bare `character(N)` cbind-ed into a `pmap` frame, and the collision is a property of a row PAIR found before `dir_walk` is entered) — added 2026-08-07 — hotfix batch-extension-case, PR #9; M18
- Four batch-outcome doc claims the M18 review scored just under its action bar: `aw_prep_audio()`'s abort-over-skip comment reasons from a premise M18 removed (75); `of_extract_dir()`'s `@return` documents a `"skipped"` state `of_extract()` has no site to produce (75); NEWS's "a missing ffprobe stops the run" is false on the `aw_transcribe_dir()` path now its `tryCatch` is gone (72); `aw_transcribe()`'s new abort is absent from its own `@return` (62) — added 2026-08-09 — M18 review round 2
- Two batch-table test gaps the M18 review scored just under its action bar: the computed-domain wrapper test runs all five `*_dir()` over an EMPTY directory, so only `dir_walk()`'s zero-row branch is exercised and a wrapper dropping `status` from populated rows stays green (58); and no populated `*_dir()` table has its column set pinned anywhere, which is what the retired `dir_walk_reports_failure()` proxy incidentally covered (48) — added 2026-08-09 — M18 review round 2
