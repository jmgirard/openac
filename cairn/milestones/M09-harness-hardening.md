# M09: Test-harness hardening — fake fidelity and a non-vacuous coverage gate

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP7, IP1
- **Branch/PR:** `m09-harness-hardening`

## Goal

Close the nine harness findings the M06, M07 and M08 reviews logged, so the
boundary harness's fakes behave like the functions they stand in for and its
coverage gate cannot pass vacuously.

## Scope

**In:** `tests/testthat/helper-openac.R` and its own test file
`test-helper-boundary.R`, plus `test-zzz-command-contract.R`'s skip condition.
One shared `Sys.which()` executability predicate, driven by the faked OS and
faithful to real `Sys.which()` on both platforms; per-platform fixture binaries;
`rappdirs` redirection folded into `local_fake_tools()`; an absolute-command
assertion inside the recorder; an unflattened `args` accessor; a computed
alias-class lock over `openac_name_of()`; and a coverage gate that fails rather
than skips when a full run records nothing.

**Out:** any change under `R/` — this milestone is test-code only, and a defect
it surfaces in package code becomes a `/hotfix` or its own milestone. GP7
layer 2 (`test-real-tools.R`, real gated invocations) is untouched: it runs the
unmocked `system2` and AC5's check cannot reach it. The `test-coverage`/Codecov
workflow stays a candidate row (it needs a repository secret). `covr`
percentages remain a diagnostic, never a gate (PROFILE `test-doctrine`).

## Acceptance criteria

- [ ] AC1: `helper-openac.R` defines exactly one executability predicate —
      "would a real `Sys.which()` resolve this path" — taking the platform as an
      explicit argument rather than reading `.Platform$OS.type` internally, and
      both `local_fake_tools()` and `local_fake_downloads()` install one shared
      `Sys.which` fake calling it with the platform their test's
      `local_fake_os()` names, not the host's. Evidence:
      `grep -c "fake_is_executable <- function\|fake_sys_which <- function"
      tests/testthat/helper-openac.R` returns 2, both at top level.
- [ ] AC2: the predicate models what R's `Sys.which()` was measured to do (M09
      probe, R 4.6.1, GitHub runners). Driven with the platform set to Windows it
      returns `TRUE` for an existing file carrying any extension — `.exe`,
      `.bat`, `.cmd`, `.com` and `.txt` all measured as resolving — `TRUE` for an
      extensionless path whose `<path>.exe` sibling exists, and `FALSE` for an
      extensionless path with no such sibling; file mode is irrelevant there.
      Driven with Unix it returns `TRUE` for an existing mode-0755 file whatever
      its extension and `FALSE` for an existing mode-0644 file, skipping that
      drive as root (`file.access(path, 1L)` returns 0 there whatever the mode).
      Both drives run wherever the suite runs, so a macOS run still exercises the
      Windows branch.
- [ ] AC3: `local_fake_tools()` creates fixture binaries carrying the extension
      the host platform requires (`.exe` on Windows, none elsewhere), the fake
      resolves a bare program name to that fixture on every platform, and
      `boundary_tools()` records the extension-stripped program name so every
      existing tool assertion holds unchanged. Evidence: `R CMD check` green on
      all five CI platforms in this milestone's PR (macOS, Windows, Ubuntu
      devel/release/oldrel-1).
- [ ] AC4: `local_fake_tools()` redirects every `rappdirs::` function openac's
      package code calls. A test enumerates those call sites by walking the
      loaded `openac` namespace for `rappdirs::user_*_dir` calls — the source
      tree is absent under `R CMD check` — and asserts each named function
      returns something other than its real value inside scope — so a future
      call site to a third `rappdirs` dir fails the test rather than leaking.
- [ ] AC5: `fake_system2()` fails the calling test when the `command` it
      receives is not absolute, decided by an explicit `is_absolute_path()`
      matching the three absolute forms — POSIX `/x`, UNC `\\server\share`, and
      a Windows drive `C:/x` or `C:\x`. Not
      `identical(path, normalizePath(path, "/", mustWork = FALSE))`, which was
      tried and is silently wrong: `normalizePath()` returns a path it cannot
      resolve unchanged, so every relative path that does not exist compared
      equal and passed — exactly the regression the check guards against. The
      check runs on every boundary call routed through `local_fake_tools()`, not
      a chosen sample. A test asserts it fires for a relative command.
- [ ] AC6: a `boundary_argv(state)` accessor returns each call's `args` exactly
      as `system2()` received it, and `boundary_args()` is defined in terms of
      it. A test asserts `boundary_argv()` distinguishes a call made with
      `c("-i", "a b")` from one made with `"-i a b"`, which `boundary_args()`
      renders identically.
- [ ] AC7: a test computes every set of openac namespace names bound to one
      identical closure and asserts `openac_name_of()` returns the recorded
      primary for each; an alias class absent from the recorded table fails the
      test naming it. The four classes today are `ffm`/`ffmpeg`, `ffp`/`ffprobe`,
      `of`/`openface`, `opensmile`/`os`.
- [ ] AC8: with owner *attribution* disabled while the harness still records
      that it ran, a full suite run makes `test-zzz-command-contract.R` FAIL
      rather than skip; `testthat::test_file()` on that file alone still skips.
      Both runs and their output go in the work log.
- [ ] AC9: `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; the standing spelling
      NOTE justified).

## Coverage

- AC1 → T1
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7
- AC8 → T8
- AC9 → T9

## Tasks

- [x] T1: replace `fake_is_executable()` ([helper-openac.R:96](tests/testthat/helper-openac.R:96))
      with one predicate taking the platform explicitly; delete
      `local_fake_downloads()`'s second `Sys.which` fake
      ([:250](tests/testthat/helper-openac.R:250)) and point both helpers at one
      shared fake that reads the OS from `local_fake_os()`'s value.
- [x] T2: test both branches in `test-helper-boundary.R`, including the root
      skip on the Unix drive.
- [x] T3: give `local_fake_tools()` fixtures the host's required extension and
      strip it in `boundary_tools()` ([:325](tests/testthat/helper-openac.R:325));
      run the suite on Windows CI before trusting it.
- [x] T4: fold `local_fake_config()` and `local_fake_data_dir()` into
      `local_fake_tools()`; add the `rappdirs::`-call-site enumeration test.
- [x] T5: add the absolute-command assertion inside `fake_system2()` and fix any
      call site it trips.
- [x] T6: add `boundary_argv()`, redefine `boundary_args()` over it, test the
      discrimination.
- [x] T7: add the computed alias-class lock.
- [x] T8: replace `test-zzz-command-contract.R`'s `skip_if(length(covered) == 0)`
      ([:80](tests/testthat/test-zzz-command-contract.R:80)) with a run-scope
      signal separate from attribution; record both runs.
- [ ] T9: `devtools::document()` if roxygen changed, `devtools::test()`,
      `devtools::check()`; retire the M08 executability lesson from LESSONS.md
      and write its replacement.

## Work log

- 2026-08-07: created by /milestone-plan; absorbs the compound harness-hardening candidate row (M06 review 2 R2/R13/R12/R3/R5; M08 review F6/F7; M07 review B1/P1).
- 2026-08-07: criteria audit ran — an [O] reader returned seven findings, all fixed at the gate: `local_fake_downloads()`'s permissive fake is load-bearing for `test-installers.R` (AC1 now drives the predicate from the faked OS); `.exe` fixtures would break ~15 `boundary_tools()` assertions on Windows (AC3 now strips the extension); AC1's evidence tested a different proposition than AC1's claim; AC8 named no mechanism and was unsatisfiable as written; AC2 contradicted itself on "every host"; AC5 over-claimed over `test-real-tools.R` and left "absolute" undefined on Windows; AC4's universal had no enumerating procedure.
- 2026-08-07: plan gate chose a Windows-faithful predicate over leaving it loose and documenting the divergence, because a fake asserting a resolution the real `Sys.which()` would refuse is the defect F6 named, not a note to keep; falsified by Windows CI going red on the fixture rename in a way that cannot be fixed inside the harness.
- 2026-08-07: plan gate chose one milestone over cutting R12 and R3 to candidate rows, because all nine findings edit the same 341-line file and two milestones in it would collide; falsified by the task list outgrowing one working session each.
- 2026-08-07: T4 done — `local_fake_tools()` now owns both rappdirs dirs and exposes them as `state$config`/`state$data`; 10 call sites in test-programs-resolve.R and test-commands-probe.R dropped their own `local_fake_config()`. The AC4 enumeration test walks `asNamespace("openac")` rather than `R/` so it still runs under `R CMD check`, where the source tree is gone; it finds `user_config_dir` and `user_data_dir`.
- 2026-08-07: T2 probe measured R 4.6.1 `Sys.which()` on GitHub runners. Windows resolves an existing file with ANY extension (`.txt` resolved at 0755 while `file.access()` reported -1, so mode is irrelevant there) or an extensionless path with a `<path>.exe` sibling; Unix resolves iff `file.access(path, 1L) == 0`, extension irrelevant. Both the plan's four-extension list and the M08 "degrade to existence" rule were wrong.
- 2026-08-07: AC2 amended at the implementation gate to the measured rule (any extension; `.exe`-sibling fallback; mode irrelevant on Windows) — the planned four-extension list was a guess and `.txt` falsified it.
- 2026-08-07: AC4 amended at the implementation gate — the enumeration walks the loaded namespace, not `R/`, because the source tree is absent under `R CMD check` and the criterion's procedure would have skipped exactly there.
- 2026-08-07: minor amendment — AC2's Unix drive skips on a Windows host as well as as root; the probe measured `file.access(<0755 extensionless>, 1L)` as -1 on Windows, so a Windows filesystem cannot represent the mode distinction the drive asserts. The predicate's unix branch degrades to existence there for the same reason.
- 2026-08-07: T1–T3 done — one measured `fake_is_executable(path, os)` plus one shared `fake_sys_which()` factory; `local_fake_downloads()`'s divergent copy deleted; fixtures carry the host's extension via `fake_program_file()` and `boundary_tools()` strips it via `fake_program_name()`. Suite 450 pass / 0 fail on macOS; Windows is CI's to confirm.
- 2026-08-07: the B1/P1 test was checked for falsifiability before being kept (M07's could-not-fail finding): for a 0644 file the old `file.exists()` rule returns TRUE where the new predicate returns FALSE, so the assertion discriminates.
- 2026-08-07: T5 found AC5's named mechanism silently wrong — `normalizePath(p, mustWork = FALSE)` returns an unresolvable path unchanged, so `identical(p, normalizePath(p))` was TRUE for every relative path that does not exist, i.e. exactly the regression it guards against. Replaced with `is_absolute_path()` matching the three absolute forms (POSIX, UNC, Windows drive); AC5's wording still to amend.
- 2026-08-07: T5-T8 done — absolute-command check inside the recorder (no existing call site tripped it), `boundary_argv()` preserving argument boundaries, computed alias-class lock over `openac_name_of()`, and a `runs` counter separating "the harness ran" from "the harness attributed".
- 2026-08-07: AC8 evidence. Full suite with attribution disabled and the harness still recording installs: FAIL 2, `test-zzz-command-contract.R:89` — "Expected owners recorded across 22 harness installs > 0". Same file alone under `testthat::test_file()`: SKIP 1, `test-zzz-command-contract.R:84` — "command contract needs the full test suite". Helper restored and suite re-run clean afterwards.
- 2026-08-07: AC5 amended at the implementation gate — `is_absolute_path()` matching POSIX/UNC/Windows-drive forms replaces the planned `normalizePath()` identity test, which did not detect a non-existent relative path; the rejected mechanism is recorded in the criterion so it is not re-proposed.
- 2026-08-07: LESSONS — retired the M06/M08 executability lesson (`test-helper-boundary.R` now fails on the mistake it warned about, D-051) and replaced it with the measured `Sys.which()` rule; added the `normalizePath()` absoluteness trap.
- 2026-08-07: local `devtools::check()` clean — 0 errors, 0 warnings, 1 NOTE, the standing spelling NOTE that already carries its own candidate row.
- 2026-08-07: first branch CI run — 4/5 platforms green, Windows failed on one assertion T3 had missed (`test-helper-boundary.R:71` read the raw `$command` basename, which is `ffmpeg.exe` there) with 466 pass / 1 fail; both predicate branches and every other assertion held. Routed that read through `fake_program_name()`. This is the failure the plan gate predicted and the reason T3 refused to trust a macOS run.
- 2026-08-07: 9 acceptance criteria exceeds the 7 tripwire deliberately — one per independent review finding plus the profile's verify slot, each separately fenceable at review; merging them would blur which finding a piece of evidence closes.

## Decisions

## Review
