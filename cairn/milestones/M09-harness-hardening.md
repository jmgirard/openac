# M09: Test-harness hardening — fake fidelity and a non-vacuous coverage gate

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP7, IP1
- **Branch/PR:** `m09-harness-hardening` · https://github.com/jmgirard/openac/pull/10

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

- [x] AC1: `helper-openac.R` defines exactly one executability predicate —
      "would a real `Sys.which()` resolve this path" — taking the platform as an
      explicit argument rather than reading `.Platform$OS.type` internally, and
      both `local_fake_tools()` and `local_fake_downloads()` install one shared
      `Sys.which` fake calling it with the platform their test's
      `local_fake_os()` names, not the host's. Evidence:
      `grep -c "fake_is_executable <- function\|fake_sys_which <- function"
      tests/testthat/helper-openac.R` returns 2, both at top level.
- [x] AC2: the predicate models what R's `Sys.which()` was measured to do (M09
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
- [x] AC3: `local_fake_tools()` creates fixture binaries carrying the extension
      the host platform requires (`.exe` on Windows, none elsewhere), the fake
      resolves a bare program name to that fixture on every platform, and
      `boundary_tools()` records the extension-stripped program name so every
      existing tool assertion holds unchanged. Evidence: `R CMD check` green on
      all five CI platforms in this milestone's PR (macOS, Windows, Ubuntu
      devel/release/oldrel-1).
- [x] AC4: `local_fake_tools()` redirects every `rappdirs::` function openac's
      package code calls. A test enumerates those call sites by walking the
      loaded `openac` namespace for `rappdirs::user_*_dir` calls — the source
      tree is absent under `R CMD check` — and asserts each named function
      returns something other than its real value inside scope — so a future
      call site to a third `rappdirs` dir fails the test rather than leaking.
- [x] AC5: `fake_system2()` fails the calling test when the `command` it
      receives is not absolute, decided by an explicit `is_absolute_path()`
      matching the three absolute forms — POSIX `/x`, UNC `\\server\share`, and
      a Windows drive `C:/x` or `C:\x`. Not
      `identical(path, normalizePath(path, "/", mustWork = FALSE))`, which was
      tried and is silently wrong: `normalizePath()` returns a path it cannot
      resolve unchanged, so every relative path that does not exist compared
      equal and passed — exactly the regression the check guards against. The
      check runs on every boundary call routed through `local_fake_tools()`, not
      a chosen sample. A test asserts it fires for a relative command.
- [x] AC6: a `boundary_argv(state)` accessor returns each call's `args` exactly
      as `system2()` received it, and `boundary_args()` is defined in terms of
      it. A test asserts `boundary_argv()` distinguishes a call made with
      `c("-i", "a b")` from one made with `"-i a b"`, which `boundary_args()`
      renders identically.
- [x] AC7: a test computes every set of openac namespace names bound to one
      identical closure and asserts `openac_name_of()` returns the recorded
      primary for each; an alias class absent from the recorded table fails the
      test naming it. The four classes today are `ffm`/`ffmpeg`, `ffp`/`ffprobe`,
      `of`/`openface`, `opensmile`/`os`.
- [x] AC8: with owner *attribution* disabled while the harness still records
      that it ran, a full suite run makes `test-zzz-command-contract.R` FAIL
      rather than skip; `testthat::test_file()` on that file alone still skips.
      Both runs and their output go in the work log.
- [x] AC9: `Rscript -e 'devtools::test()'` clean and `Rscript -e
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
- [x] T9: `devtools::document()` if roxygen changed, `devtools::test()`,
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
- 2026-08-07: AC3 evidence — branch CI run 31238766612 green on all five platforms (macOS release, Windows release, Ubuntu devel/release/oldrel-1).
- 2026-08-07: temporary `probe-syswhich.yaml` deleted; the PR makes `R-CMD-check.yaml` run on the same five platforms. Its measurements survive as the comment on `fake_is_executable()` and the M09 LESSONS entry.
- 2026-08-07: T9 done; all criteria met. Status → review.
- 2026-08-07: 9 acceptance criteria exceeds the 7 tripwire deliberately — one per independent review finding plus the profile's verify slot, each separately fenceable at review; merging them would blur which finding a piece of evidence closes.

## Decisions

## Review

**Evidence gathered 2026-08-07 on branch `m09-harness-hardening`, PR #10.**

Process note: the criterion checkboxes were ticked at implement-completion, before this Review section existed — AC fencing puts that tick here, against recorded evidence. The evidence below was gathered fresh at review and backs every tick, but the ordering was wrong and is recorded rather than papered over.

- AC1 — `grep -c "fake_is_executable <- function\|fake_sys_which <- function" tests/testthat/helper-openac.R` returns **2**, both at top level (helper-openac.R:125 and :142). `local_fake_downloads()` installs `fake_sys_which()` with no arguments; `local_fake_tools()` installs it with `resolve`/`bindir`. Neither defines a fake of its own.
- AC2 — `test-helper-boundary.R` drives both branches by argument: Windows accepts `.exe`/`.bat`/`.cmd`/`.com`/`.txt` and a 0644 file with an extension, refuses an extensionless path, and accepts it once a `<path>.exe` sibling exists; Unix accepts 0755 and refuses 0644 whatever the extension. 58 pass / 0 fail on macOS, so the Windows drives ran off-platform as intended. The Unix drives skipped on the Windows runner (`test-helper-boundary.R:143`, `:182` reported "On Windows") and ran everywhere else.
- AC3 — fixtures carry `fake_program_file()`; `boundary_tools()` normalizes via `fake_program_name()`. Branch CI run 31238766612: **green on all five platforms** (macOS release, Windows release, Ubuntu devel/release/oldrel-1). The preceding run caught one un-normalized read (`test-helper-boundary.R:71`), which was fixed.
- AC4 — the enumeration walks `asNamespace("openac")` and finds `user_config_dir` and `user_data_dir`, asserting each differs from its real value inside `local_fake_tools()` scope. `expect_gt(length(used), 0)` guards the walk against silently matching nothing.
- AC5 — `is_absolute_path()` verified against `/usr/bin/ffmpeg`, `C:/x/ffmpeg.exe`, `C:\x\ffmpeg.exe`, `\\srv\share\t.exe` (all TRUE) and `ffmpeg`, `rel/ffmpeg`, `./ffmpeg` (all FALSE). The check sits in `fake_system2()` before the queue index advances; no existing call site tripped it. A test asserts it fires for a bare name and for a relative path.
- AC6 — `boundary_argv()` returns `list(c("-i", "a b"), "-i a b")` for two calls that `boundary_args()` renders identically; the test asserts both halves.
- AC7 — the alias classes are computed by grouping namespace names on closure identity, yielding exactly the four expected; `expect_setequal()` against the recorded table fails by name on an unrecorded class, and `openac_name_of()` is asked via every binding in each class.
- AC8 — with attribution disabled and installs still recorded: full suite FAILS at `test-zzz-command-contract.R:89` — "Expected owners recorded across 22 harness installs > 0" (FAIL 2). The same file alone SKIPS at `:84` — "command contract needs the full test suite" (FAIL 0, SKIP 1). Helper restored, suite re-run clean.
- AC9 — `devtools::test()`: **486 pass, 0 fail**, 2 skips (both `test-real-tools.R` binary gates). `devtools::check()`: **0 errors, 0 warnings, 1 NOTE** — the standing spelling NOTE, which carries its own ROADMAP candidate row.

**Consistency gate.** `cairn_validate.py` exit 0, all checks passed (1 advisory: the deliberate 9-criteria sizing tripwire, justified in the work log). `devtools::document()` produces no diff. No `_pkgdown.yml` in this repo. README.Rmd/README.md unchanged by this milestone. No principle changed, so `cairn_impact` does not apply. No `Driving RR`, so no projection-vs-outcome pairs.
