# M08: GitHub Actions CI — R CMD check across platforms

**Status:** done (2026-08-07, PR #7 https://github.com/jmgirard/openac/pull/7)

**Goal:** Give the repo a working `R CMD check` workflow on Windows, macOS and
Linux, so the review gate's never-merge-red-CI rule has something to read.

**Outcome:** usethis `check-standard` workflow — macOS, Windows, Ubuntu
devel/release/oldrel-1, on `pull_request` and push to `main`, `error-on`
inherited as `"warning"` from `check-r-package@v2`. Departs from the template
with `dependencies: '"hard"'` + named extras and `_R_CHECK_FORCE_SUGGESTS_:
false`, so no runner installs the `Remotes:`-backed `audio.whisper` or compiles
whisper.cpp, plus a guard step aborting the job if it appears. Also
`^\.github$` in `.Rbuildignore`, a README badge, `CMD` in `inst/WORDLIST`.
First run failed on Windows only: `fake_sys_which()` gated on POSIX-only
`file.access(n, 1L)`, so extensionless 0755 fixtures never resolved;
`fake_is_executable()` now degrades to existence there. No `R/` code changed.

**Decisions:** none. The `test-coverage`/Codecov workflow and branch protection
were scoped out to candidate rows at plan time.

**Review:** Three lenses, 15 findings, none ≥80 — actioned list empty, no
return floor met; two premises verified false. F6/F7 (`fake_is_executable()`
looser than real `Sys.which()`; neither branch tested) absorbed into the
harness-hardening candidate row. Nothing retired from LESSONS.
