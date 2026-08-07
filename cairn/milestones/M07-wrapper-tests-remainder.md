# M07: Wrapper testing contract — remainder and gated real invocations

- **Status:** planned
- **Priority:** normal
- **Depends on:** M06
- **Driving RR:** —
- **Principles touched:** GP3, GP6, GP7, IP1

- **Branch/PR:** —

## Goal

Empty M06's deferral list and add GP7's second layer — real tool invocations
behind skip gates.

## Scope

**In:** command/URL-construction tests for the `install_*` family with network
and archive extraction mocked; OS guards on the platform-specific installers;
tests for the `*_dir` batch wrappers (enumeration, output-path derivation, GP6
skip-and-report) including the path-derivation fixes they surface;
`aw_transcribe`/`aw_transcribe_wav` against a mocked `audio.whisper`; and a
gated real-invocation layer.

**Out:** installer *dispatchers* (`install_openface()` detecting the OS and
delegating) → candidate row, since they add exports and amend DESIGN's
`_win`/`_mac` convention and so need a D-entry. GP8 runtime reporting →
candidate row. Replacing the OneDrive model URLs → candidate row. CRAN
submission → user-declared release window (D-050).

## Acceptance criteria

- [ ] AC1 The deferral list in `test-command-contract.R` is empty and the test
      passes with its computed domain covering the full transitive closure — so
      every openac function that can reach `system2` records a command
      assertion, with no name exempted.
- [ ] AC2 Each of `install_ffmpeg_win`, `install_openface_win`,
      `install_openface_mac`, `install_opensmile_win`, `install_opensmile_mac`
      and `install_whisper` has tests asserting the download URL and install
      path it constructs, with `utils::download.file` and
      `archive::archive_extract` mocked to record their arguments and to fail
      the test if called with an unexpected URL or destination. The mocks are
      the procedure that establishes no test performs a real network request or
      writes outside a per-test temporary directory.
- [ ] AC3 Every installer whose name carries a `_win` or `_mac` suffix — the
      set computed from `getNamespaceExports("openac")` by suffix match, not a
      hand list — aborts with a classed `cli` condition when `Sys.info()`
      reports a different OS, tested in both directions per installer. On an OS
      with no installer for a tool, the message names the tool and states that
      no automated installer exists rather than failing silently.
- [ ] AC4 For each `*_dir` wrapper, tests cover: extension matching (including
      that a file named `clip.mp4.backup.mp4` and a directory named `mp4` do not
      mis-derive), `recursive` on and off, output-path derivation for an input
      directory containing regex metacharacters, and GP6 skip-and-report when
      the per-file operation fails on one file of several. The derivation bugs
      these surface (`gsub(indir, outdir, …)` treating a path as a regex,
      unanchored `gsub(inext, …)`, and the inconsistent `paste0(inext, "$")`
      vs `paste0("\\.", inext, "$")` patterns) are fixed (D-002 permits the
      behavior change).
- [ ] AC5 `aw_transcribe()` and `aw_transcribe_wav()` are tested with
      `audio.whisper`'s `predict` mocked, asserting the parameters passed
      through and the file written, with no model download and no whisper run.
- [ ] AC6 `tests/testthat/test-real-tools.R` performs at least one real
      invocation per wrapped tool behind `skip_if(!check_<tool>())` and
      `skip_on_cran()`; `devtools::test()` passes both with the tools installed
      and with resolution forced to fail, and `devtools::check()` reports 0
      errors and 0 warnings with NOTEs measured against M06's recorded baseline.

## Coverage

- AC1 → T7
- AC2 → T2
- AC3 → T1
- AC4 → T3, T4
- AC5 → T5
- AC6 → T6, T8

## Tasks

- [ ] T1 Add OS guards to the `_win`/`_mac` installers plus the
      no-installer-for-this-OS message; test both directions with `Sys.info()`
      mocked.
- [ ] T2 Extend `helper-openac.R` with `download.file` / `archive_extract`
      mocks; write the `install_*` URL and install-path tests.
- [ ] T3 Fix the `*_dir` path derivation — anchor the extension pattern, stop
      treating `indir` as a regex, derive output paths without a global `gsub`.
- [ ] T4 Tests for the `*_dir` wrappers: enumeration, `recursive`, metacharacter
      directories, GP6 skip-and-report with the per-file operation mocked.
- [ ] T5 Tests for `aw_transcribe`/`aw_transcribe_wav` with `audio.whisper`'s
      `predict` mocked.
- [ ] T6 Write `test-real-tools.R` — one gated real invocation per tool behind
      `skip_if(!check_<tool>())` + `skip_on_cran()`.
- [ ] T7 Empty the deferral list in `test-command-contract.R`; confirm the
      staleness assertion and the full-closure gate pass.
- [ ] T8 Run `document()`, `test()` (both tool states), `check()`; compare
      NOTEs against M06's baseline; fix fallout.

## Work log

- 2026-08-07: created by /milestone-plan alongside M06.
- 2026-08-07: criteria audit [O] ran on M06's criteria block, which fixed this milestone's domain boundary; M07's criteria inherit its two structural repairs — literal deferral names with a staleness assertion, and a symbol-occurrence closure, since `os_extract_dir` and `aw_transcribe_dir` reach their tools via `do.call(what = …)` and are invisible to a call-head walk, which would let AC1's "deferral list empty" gate pass with them outside the domain.
- 2026-08-07: plan gate chose OS guards here over installer dispatchers because guards are a correctness fix for a verified defect (no `Sys.info()` or `.Platform` check exists anywhere in `R/programs_install.R`, so `install_opensmile_win()` on macOS extracts Windows binaries and reports success) while dispatchers add exports and amend a stated DESIGN convention; falsified by the user preferring a single platform-aware entry point over the suffixed family.

## Decisions

## Review
