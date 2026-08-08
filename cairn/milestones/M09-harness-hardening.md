# M09: Test-harness hardening — fake fidelity at the tool boundary

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP7, IP1
- **Branch/PR:** `m09-harness-hardening` · https://github.com/jmgirard/openac/pull/10

## Goal

Close the harness-fidelity findings the M06, M07 and M08 reviews logged, so the
boundary harness's fakes behave like the functions they stand in for. The
coverage gate the original cut also carried is M10.

## Scope

**In:** `tests/testthat/helper-openac.R` and its own test file
`test-helper-boundary.R`, plus the call-site updates those changes force in
`test-programs-resolve.R` and `test-commands-probe.R`. One shared `Sys.which()`
executability predicate, driven by the faked OS and faithful to real
`Sys.which()` on both platforms; per-platform fixture binaries; `rappdirs`
redirection folded into `local_fake_tools()`; an absolute-command assertion
inside the recorder; an unflattened `args` accessor; and a computed alias-class
lock over `openac_name_of()`.

**Out:** any change under `R/` — this milestone is test-code only, and a defect
it surfaces in package code becomes a `/hotfix` or its own milestone. The
command-contract coverage gate and everything RR02 binds is **M10**; this
milestone reverts `test-zzz-command-contract.R` and its support machinery to the
default branch's state so M10 starts from one baseline. GP7
layer 2 (`test-real-tools.R`, real gated invocations) is untouched: it runs the
unmocked `system2` and AC5's check cannot reach it. The `test-coverage`/Codecov
workflow stays a candidate row (it needs a repository secret). `covr`
percentages remain a diagnostic, never a gate (PROFILE `test-doctrine`).

## Acceptance criteria

- [x] AC1: `helper-openac.R` has exactly one executability rule,
      `fake_sys_which_path()`, taking the platform as an explicit argument rather
      than reading `.Platform$OS.type`; `fake_is_executable()` is a single call to
      it, not a second copy; both `local_fake_tools()` and
      `local_fake_downloads()` install one shared `Sys.which` fake driven by the
      platform their test's `local_fake_os()` names, not the host's. Evidence:
      `grep -c "^fake_sys_which_path <- function\|^fake_is_executable <- function"
      tests/testthat/helper-openac.R` returns 2.
- [x] AC2: the rule matches what `Sys.which()` was MEASURED to do (M09 probes,
      R 4.6.1, GitHub runners). Windows: TRUE for an existing file with any
      extension (`.exe`/`.bat`/`.cmd`/`.com`/`.txt` all measured); for an
      extensionless path, TRUE iff a `.com`/`.exe`/`.bat`/`.cmd` sibling exists —
      whether or not the path itself does — returning that sibling, and FALSE for
      a `.txt` sibling or none; mode irrelevant. Unix: TRUE at mode 0755, FALSE at
      0644, extension irrelevant. The Windows drives run wherever the suite runs;
      the Unix drive skips as root and on a Windows host. One deliberate, tested
      divergence: the real Windows `Sys.which()` returns a DIRECTORY named
      `tool.exe`; the fake refuses it, since openac would hand it to `system2()`.
- [x] AC3: fixtures carry the extension the SIMULATED platform requires (`.exe`
      when the test's `local_fake_os()` names Windows, or unfaked on a Windows
      host), the fake resolves a bare name to that fixture on every platform, and
      `boundary_tools()` strips it so every existing assertion holds. Evidence:
      `R CMD check` green on all five CI platforms in this milestone's PR.
- [x] AC4: `local_fake_tools()` redirects every `rappdirs::` function openac's
      code calls, enumerated by walking the loaded namespace for
      `rappdirs::user_*_dir` (the source tree is absent under `R CMD check`) and
      asserting each differs from its real value in scope, so a third dir fails
      the test rather than leaking.
- [x] AC5: `fake_system2()` fails the calling test when `command` is not
      absolute, decided by `is_absolute_path()` matching POSIX `/x`, UNC
      `\\server\share`, and Windows `C:/x` or `C:\x`. NOT
      `identical(path, normalizePath(path, "/", mustWork = FALSE))`, which returns
      an unresolvable path unchanged and so passed every relative path that does
      not exist. Runs on every call routed through `local_fake_tools()`, not a
      sample; a test asserts it fires.
- [x] AC6: `boundary_argv(state)` returns each call's `args` as `system2()`
      received it and `boundary_args()` is defined over it; a test asserts
      `c("-i", "a b")` is distinguished from `"-i a b"`.
- [x] AC7: a test computes every set of namespace names bound to one identical
      closure and asserts `openac_name_of()` returns the recorded primary; an
      unrecorded class fails by name. Four today: `ffm`/`ffmpeg`, `ffp`/`ffprobe`,
      `of`/`openface`, `opensmile`/`os`.
- [x] AC8: `Rscript -e 'devtools::test()'` clean and `Rscript -e
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
- AC8 → T9, T14

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
- [x] T14: revert `test-zzz-command-contract.R` and its support machinery (`openac_registry$runs`/`$files`, `harness_runs()`, `harness_files()`, `harness_caller_file()`, the install-site recording, and `test-helper-boundary.R`'s recording test) to the default branch's state — the gate is M10's.
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
- 2026-08-07: return gate — AC8 amended to name the partial-run case O15 found: the gate must decide from run completeness, not from what was recorded, so `devtools::test(filter = "helper-boundary|zzz")` skips instead of failing. Scope "In:" amended to name `test-programs-resolve.R` and `test-commands-probe.R`, which the diff rewrote (O24).
- 2026-08-07: return gate chose to MEASURE AC2's `<path>.exe` sibling rule on Windows CI rather than accept it as inferred — the first probe created `tool` and `tool.exe` together, so the sibling-only case it asserts was never observed. Second probe workflow pushed; AC2's text waits on its output.
- 2026-08-07: AC1 met (O3) — `fake_program_file()` reads the simulated platform, the same source the predicate reads, and `local_fake_tools()` captures it once (`os` argument, `state$os`) and hands it to both the fixture namer and the resolver, so the tree and its resolver cannot disagree. New test drives `local_fake_os("Windows")` on a Unix host: fixtures carry `.exe`, `Sys.which()` returns them, `boundary_tools()` still reads `ffmpeg`. Falsified against the old host-reading namer: 3 failures.
- 2026-08-07: AC2 met (O1) — second Windows probe, one file per directory, always asking for the extensionless name. `.exe`/`.bat`/`.cmd`/`.com` siblings resolve and return the SIBLING's path; `.txt` sibling and no sibling do not; a 0644 `.exe` still resolves. The predicate is now `fake_sys_which_path()` with `fake_is_executable()` defined over it, so the resolver returns what the platform would return instead of the name asked for. Falsified against the reinstated `!file.exists()` guard: 2 failures.
- 2026-08-07: probe measured one divergence to keep — real Windows `Sys.which()` RETURNS a directory named `tool.exe`. The predicate refuses directories, now recorded in AC2 and tested, because openac would hand that to `system2()`.
- 2026-08-07: amendment return: AC2 — "The Windows drives run wherever the suite runs, so a macOS run still exercises them; the Unix drive is skipped as root (`file.access(path, 1L)` returns 0 there whatever the mode) and on a Windows host, which has no mode bit for it to read."
- 2026-08-07: AC8 met (O15) — the gate now reads run COMPLETENESS, not install count: `local_fake_tools()` records the test file it was called from (call-stack srcref), and `harness_test_files()` reads the expected set off the test directory by searching for the helper's name, assembled at runtime so the contract file does not match itself. Three runs on a healthy tree — full suite: FAIL 0, PASS 504, the contract enforcing; `filter = "helper-boundary|zzz"`: FAIL 0, SKIP 1 at `test-zzz-command-contract.R:129` — "command contract needs the full test suite (1 of 7 harness files ran; missing test-batch-dirs.R, test-commands-extract.R, test-commands-prep.R, test-commands-probe.R, test-programs-resolve.R, test-whisper-transcribe.R)"; that file alone under `test_file()`: FAIL 0, SKIP 1 at `:117`. The broken-attribution case is unchanged and still fails.
- 2026-08-07: sub-threshold findings fixed in passing, all inside code this return already touched — O2 (the sibling rule returned the requested name, not the sibling), O4/B1 (`local_fake_downloads()` docstring still claimed it resolved any existing file), O5 (recorder comment still named `normalizePath`), O18 (queue-exhausted message used the unstripped basename). The remaining logged findings stand.
- 2026-08-07: LESSONS — the executability entry rewritten to the measured sibling set (`.com`/`.exe`/`.bat`/`.cmd`, returning the sibling's path); added the probe-design lesson that one directory per case is what makes a probe able to say which file answered.
- 2026-08-08: AC3 amended at the return gate — "the SIMULATED platform requires", not the host's, which the O3 fix made false. Behavior is identical wherever no platform is faked, which is every pre-existing test.
- 2026-08-08: AC1 note for review — the executability RULE now lives in exactly one function, `fake_sys_which_path()`, and `fake_is_executable()` is a one-line view over it rather than a second copy. AC1's evidence grep still returns 2, both at top level (helper-openac.R:184 and :230).
- 2026-08-08: local `devtools::check()` clean after the return work — 0 errors, 0 warnings, 1 NOTE, the standing spelling NOTE. Suite 504 pass / 0 fail, 2 skips (both `test-real-tools.R` binary gates). Second probe workflow deleted; its measurements survive in the comment on `fake_sys_which_path()` and the M09 LESSONS entries.
- 2026-08-08: AC3 evidence after the return work — branch CI run 31241019278 (commit cce4012) green on all five platforms: macOS release, Windows release, Ubuntu devel / release / oldrel-1. This is the run that exercises the measured sibling rule and the simulated-platform fixture namer on a real Windows host.
- 2026-08-08: all criteria met again; status -> review. The `## Review` section below still holds the evidence gathered before the return and is stale for AC1/AC2/AC3/AC8 — review regathers it fresh.
- 2026-08-08: review RETURNED at the independent-review gate, second defect return. AC8 fails on O6 (92) and O7 (90), both reproduced: `harness_test_files()` decides membership by searching each test file's TEXT for `local_fake_tools(`, so a bare mention in a COMMENT adds a file to the expected set that can never join the ran set, and the coverage gate skips silently and permanently on every run thereafter — reproduced by appending one comment line to `test-installers.R`. The same hole opens when a file's only harness installs sit behind a skip (reproduced: "6 of 7 harness files ran"), which `test-whisper-transcribe.R` will hit on any machine without the GitHub-only `audio.whisper`. This is the vacuity D-010 forbids, relocated rather than removed. AC1 fails on O4 (85) as an amendment return: after the return work the executability rule lives in `fake_sys_which_path()` (helper-openac.R:198), which AC1's evidence grep does not count — the grep's "2" now counts a one-line view and a mock factory, so it cannot detect the duplication AC1 exists to prevent. O15 (93) is triage-fix, not a criterion failure: `fake_sys_which()`'s `os` default is a cached promise, so a `local_fake_os()` call after the first `Sys.which()` is silently ignored while `local_fake_downloads()`'s docstring claims otherwise. AC1/AC8 unticked. Defect returns for this milestone: 2.
- 2026-08-08: thrash rule (b) fires — AC8 has now failed twice, each time because its run-completeness signal is a proxy that does not mean what the gate takes it to mean (return 1: installs counted as completeness; return 2: a text match counted as a harness call site). The plan gate recorded no alternative for AC8's mechanism, so escalation via `/milestone-brief` is offered per D-004. Trigger (a) does not fire: this is the second return, not the third.
- 2026-08-08: process defect in this review, recorded rather than papered over — the orchestrator ran in-place falsification patches on `helper-openac.R` in the working tree the three fresh-context reviewers share. Two lenses (P1, B1) read the deliberately broken file mid-run and reported a non-existent `keep.source` flakiness bug, each having lost its primary finding to the corruption. Verified refuted: `keep.source` is FALSE during the test run in both invocation paths and the mechanism works regardless; 8 consecutive plain-`Rscript` runs on a verified-clean tree were 8/8 green with the helper blob unchanged. B2 was refuted too — M07's review did record B1 (35) and P1 (33) for this concern, in the full text at commit ccc47ad; the lens read only the compressed archive.
- 2026-08-08: a verified fix direction for O6 exists — detecting the call sites by PARSING each test file and walking for the `local_fake_tools` symbol, rather than searching its text, yields exactly the seven real harness files and ignores comment mentions. O7 is not addressed by that change and needs its own answer.
- 2026-08-08: blocked on RB02 — AC8's run-completeness signal escalated after two failures of the same shape; the thrash rule (b) offer was taken at the return gate.
- 2026-08-08: ingested RR02. Its answer: completeness is the right precondition (the contract is a global absence claim, undecidable from a partial run), but both failures were failures of INFERENCE. `ran` becomes an observation — a `test_that` shadow recording at execution time — and `expected` a content-free ground truth, with the runner declaring full runs. RR02 rejected the parse-tree fix this session proposed at the return gate: it closes the comment hole but keeps the content-proxy shape that produced both failures. Load-bearing fact: testthat's `filter` selects whole files, never individual tests, so "every test file executed" is a biconditional for completeness rather than a proxy.
- 2026-08-08: amendment return: AC1 — "Evidence: exactly one function decides executability — `fake_sys_which_path()` — and `fake_is_executable()` is defined over it rather than restating the rule; `grep -c \"^fake_sys_which_path <- function\|^fake_is_executable <- function\" tests/testthat/helper-openac.R` returns 2, and the body of `fake_is_executable()` is a single call to `fake_sys_which_path()`."
- 2026-08-08: binding-criteria audit ran before ingestion — an [O] reader returned one UNSATISFIABLE criterion and nine needing rewording, all recorded in the Deviations table. The decisive find: BC4 forbade a contract skip that BC6's own decision table requires, because `devtools::test()` never executes `tests/testthat.R` and so never declares a full run. Verified independently, as were BC7's dead grep alternative and BC10's machine-dependent pass floor.
- 2026-08-08: RR02 corrected a factual error in this session's return — `test-whisper-transcribe.R` does NOT depend on `audio.whisper`; it mocks `predict` in openac's namespace and its header says so. audio.whisper is absent on this machine and the file runs with the gate enforcing; the skip I attributed to it belongs to `test-real-tools.R`. The conditional-skip hole class is real and was reproduced artificially, but has no live instance. The corrected doctrine is stronger: because the boundary is fully mocked, a command test never has a legitimate reason to conditionally skip, so the right behavior is to fail naming functions rather than to accommodate it.
- 2026-08-08: Scope "In:" amended to name `tests/testthat.R`, the decision-function unit tests, and the runtime-generated fixture suites — the audit flagged that the frozen "In:" is an enumeration and RR02's work falls outside it, which is what O24 returned this milestone for last pass.
- 2026-08-08: RE-CUT by /milestone-plan — the coverage gate and everything RR02 binds moved to M10 (planned, high, depends on M09). M09 keeps the fake-fidelity work and is retitled. The re-cut was forced by three agreeing sizing signals after RR02's ingestion: 12 acceptance criteria against a 7 tripwire, 13 tasks against 10, and 202 plan-owned lines against a 150 cap that survived the one mandated compression pass.
- 2026-08-08: thrash rule — a re-cut increments the defect-return count and never resets it, so M09 stands at 3 and trigger (a) is at its threshold. The split IS the prescribed remedy and is being applied now; what it means going forward is that a further return on M09 may NOT be answered with another re-plan — the remaining moves are `/milestone-brief` escalation, parking as `blocked` with the blocker named, or dropping at the user's explicit decision.
- 2026-08-08: re-cut gate chose reverting the gate to the default branch's state over merging the current one as an interim, because the current gate carries the reproduced comment hole and M10 replaces it wholesale; merging it would ship a defect review has already confirmed and named. Falsified if the revert turns out to regress something the fidelity work depends on, which T14 must check.
- 2026-08-08: T14 done — `test-zzz-command-contract.R` restored to the default branch byte-for-byte (`git diff origin/main` on it is empty), and its support machinery removed from the harness: `openac_registry$runs`/`$files`, `harness_runs()`, `harness_files()`, `harness_caller_file()`, the install-site recording inside `local_fake_tools()`, and `test-helper-boundary.R`'s recording test. `grep -rn` over `tests/` finds no surviving reference to any of them. Nothing the fidelity work depends on regressed: the suite is 502 pass / 0 fail, 2 skips (both `test-real-tools.R` binary gates).
- 2026-08-08: question gate chose to fix review finding O15 (93, DEFECT) here rather than defer it — M10's plan does not mention it, so deferring would have dropped a reproduced defect in a file this milestone rewrote, and AC1's own clause about `local_fake_downloads()` reading the platform `local_fake_os()` names was only order-dependently true while the bug stood. `fake_sys_which()`'s `os` is now `NULL`-defaulted and resolved per call instead of by a signature default, which R forces once and caches. Falsified by reinstating the cached default: `test-helper-boundary.R:257` fails, `ask("Windows")` returning `""` instead of the `.exe` sibling.
- 2026-08-08: AC1 met — `grep -c "^fake_sys_which_path <- function\|^fake_is_executable <- function" tests/testthat/helper-openac.R` returns 2 (`:156`, `:165`, both top level), and the body of `fake_is_executable()` is the single line `!identical(fake_sys_which_path(path, os), "")`. No other function in the file decides executability: `file.access()` and `fake_win_exts()` appear inside `fake_sys_which_path()` and in comments only, plus `fake_program_name()`'s strip pattern, which is a namer, not a predicate. Both helpers install `fake_sys_which()`, which reads the simulated platform on every call.
- 2026-08-08: LESSONS — added the R default-argument lesson O15 taught (a default is a promise forced once and cached, so a factory defaulting to mutable global state pins it at the closure's first call).
- 2026-08-08: all criteria met after the re-cut; status -> review. `devtools::test()` 502 pass / 0 fail / 2 skips, `devtools::check()` 0 errors / 0 warnings / 1 NOTE (the standing spelling NOTE). The `## Review` section below predates the re-cut and is stale throughout — the criteria it annotates were renumbered (its "AC8" is the removed coverage gate; its "AC8 (was AC9)" is today's AC8) — so review regathers every line rather than reading it.
- 2026-08-08: AC3 evidence on the final tree — branch CI run 31266960014 (commit 7055911) green on all five platforms: macOS release, Windows release, Ubuntu devel / release / oldrel-1. This is the run that exercises the O15 per-call platform read on a real Windows host.
- 2026-08-08: review pass 3 gathered fresh evidence for all eight criteria against 8fbdbd2; consistency gate clean (`cairn_validate` exit 0, `document()` no diff, CI 31267067836 green on five platforms). Three lenses returned 25 findings, top score 55, so nothing was actioned and the gate PASSED. The stale pre-re-cut evidence in `## Review` was replaced rather than annotated, since its criterion numbering no longer matched.
- 2026-08-08: at the merge gate the user took the flag-first option on review finding O16 (55) — D-014 appended, annotating D-013 to say its present-tense consequences specify what M10 implements rather than describe the merged tree. D-013 itself is unedited (IP4).
- 2026-08-07: 9 acceptance criteria exceeds the 7 tripwire deliberately — one per independent review finding plus the profile's verify slot, each separately fenceable at review; merging them would blur which finding a piece of evidence closes.

## Decisions

- 2026-08-07: review RETURNED at the independent-review gate. AC2 fails on O1 — the `<path>.exe` sibling branch is unreachable because the `!file.exists(path)` guard precedes it, so `fake_is_executable(".../SMILExtract", os = "Windows")` returns FALSE where the criterion requires TRUE; the clause was inferred, not measured, since the probe created both `tool` and `tool.exe`. AC8 fails on O15 — `devtools::test(filter = "helper-boundary|zzz")` FAILs on a healthy tree. AC1 fails on O3 — `fake_program_file()` reads the host platform while the predicate reads the simulated one. AC2 also needs a gated text amendment (O9): "Both drives run wherever the suite runs" is contradicted by `skip_on_os("windows")`. AC1/AC2/AC3/AC8 unticked. Defect returns for this milestone: 1.

- 2026-08-08 (RR02): completeness is established by observation plus declaration, never inference. Recorded as D-013; see the Deviations table for every departure from RR02's criteria as written.

## Review

**Evidence gathered 2026-08-08 on branch `m09-harness-hardening` at 8fbdbd2, PR #10.**
This replaces the evidence gathered before the re-cut, which annotated a different set of criteria (its "AC8" was the coverage gate now owned by M10). Every line below was re-executed today against the current tree; nothing is carried over.

- AC1 — `grep -c "^fake_sys_which_path <- function\|^fake_is_executable <- function" tests/testthat/helper-openac.R` returns **2**, both at top level (`:156`, `:165`). The body of `fake_is_executable()` is the single expression `!identical(fake_sys_which_path(path, os), "")` — read back via `deparse(body())`, not by eye — so the rule is stated once and viewed once, never restated. Both take `os` as an explicit argument. No second executability decision exists in the file: `file.access()` and `fake_win_exts()` occur inside `fake_sys_which_path()` and in comments, plus `fake_program_name()`'s strip pattern, which is a namer. The one `.Platform$OS.type` read is inside `fake_sys_which_path()`'s unix branch and decides HOST CAPABILITY (a Windows filesystem has no mode bit), never which platform to simulate. Both scoped helpers install `fake_sys_which()` and neither defines a fake: `local_fake_tools()` at `:336` passes the simulated platform it captured; `local_fake_downloads()` at `:431` passes none and the closure resolves `Sys.info()[["sysname"]]` per call. Driven live: under a simulated Windows OS `local_fake_tools()` reports `state$os` Windows and resolves `ffmpeg` to `ffmpeg.exe`; `local_fake_downloads()` resolves a bare path to `""` when Linux is simulated and to `tool.exe` when Windows is, and one closure asked twice across a platform change answers for each — the per-call read the O15 fix installed.
- AC2 — every clause driven by argument on this macOS host, so the Windows drives ran off-platform, one case per directory, always asking the extensionless name. Sibling `.com`/`.exe`/`.bat`/`.cmd` → TRUE, and each returns the SIBLING's own path (checked by identity against `paste0(path, ext)`, not just for non-emptiness); `.txt` sibling → FALSE; no sibling → FALSE. Named directly: `.exe` at mode 0644 → TRUE and `.txt` → TRUE, so the extension is what matters and the mode is not. A DIRECTORY named `tool.exe` → FALSE both when named directly and as a sibling — the one deliberate divergence from what the real Windows `Sys.which()` was measured to do. Unix: 0755 → TRUE, 0644 → FALSE, 0755 `.txt` → TRUE. The rules are measured (probe 31240405772, R 4.6.1, `windows-latest`), not inferred. The Unix drive skips as root and on a Windows host, as the criterion states.
- AC3 — under a simulated Windows OS on this macOS host the fixture tree is `ffmpeg.exe`, `ffprobe.exe`, `openface.exe`, `opensmile.exe` and `boundary_tools()` still reads `ffmpeg`; with no platform faked the same tree is extensionless and `boundary_tools()` is unchanged. So the extension follows the SIMULATED platform, and every pre-existing assertion is untouched wherever nothing is faked. CI run **31267067836 on HEAD (8fbdbd2)**: green on all five platforms — macOS release, Windows release, Ubuntu devel / release / oldrel-1. That is the run in which a real Windows host exercises the measured sibling rule.
- AC4 — walking `asNamespace("openac")` for `user_*_dir` finds exactly two, `user_config_dir` and `user_data_dir`; inside `local_fake_tools()` scope each returns a value differing from its real one. The walk reads the loaded namespace rather than `R/`, so it still runs under `R CMD check`, where the source tree is absent. A third dir would join the enumeration and fail the assertion rather than leak.
- AC5 — `is_absolute_path()` driven directly: `/usr/bin/ffmpeg`, `C:/x/f.exe`, `C:\x\f.exe`, `\\srv\share\t.exe` all TRUE; `ffmpeg`, `rel/ffmpeg`, `./ffmpeg`, `../ffmpeg` all FALSE. The rejected mechanism was re-measured and is still wrong: `identical("rel/ffmpeg", normalizePath("rel/ffmpeg", "/", mustWork = FALSE))` is TRUE. The check sits inside `fake_system2()` ahead of `state$i <- state$i + 1L`, so it sees every call any test routes through the harness rather than a sample; `test-helper-boundary.R:51` asserts it fires for a bare name and for a relative path.
- AC6 — two calls differing only in argument boundaries: `boundary_argv()` returns `[-i|a b]` and `[-i a b]`, while `boundary_args()` renders both as `-i a b`. So the accessor distinguishes what collapse erases, and `boundary_args()` is defined over it.
- AC7 — grouping every namespace name on closure identity yields exactly four alias classes — `ffm`/`ffmpeg`, `ffp`/`ffprobe`, `of`/`openface`, `opensmile`/`os` — and `openac_name_of()` returns the recorded primary when asked via either binding of each. The classes are computed, so a fifth would fail by name rather than pass unnoticed.
- AC8 — `devtools::test()`: **502 pass, 0 fail**, 2 skips (both `test-real-tools.R` binary gates, absent binaries). `devtools::check()`: **0 errors, 0 warnings, 1 NOTE** — the standing spelling NOTE, which carries its own ROADMAP candidate row.

**Consistency gate.** `cairn_validate.py` exit 0 — all 16 checks PASS, 8 advisories OK, 1 advisory WARN: the sizing tripwire (8 criteria against 7), justified in the work log as one criterion per independent review finding plus the profile's verify slot. `devtools::document()` run and produces no diff. No `_pkgdown.yml` in this repo. The diff touches nothing under `R/`, `man/`, `NAMESPACE`, `DESCRIPTION`, `README*` or `NEWS.md`, and adds no top-level file — this milestone is test-code only, so the changelog slot has nothing user-visible to record and the `.Rbuildignore` check has nothing to catch. No principle changed, so `cairn_impact` does not apply. No `Driving RR`, so no projection-vs-outcome pairs. Plan-owned body 118/149 lines.

## Independent review (2026-08-08, third pass — post-re-cut)

Three fresh-context lenses, then a scorer that generated none of the findings and was given the diff and the milestone file.
Diff-bug lens [O]: 20 findings. Blame-history lens [S]: 5 (one substantive, four explicit negative results).
Prior-review lens [S]: **0** — it traced every actioned finding from both earlier passes through the current diff and found none regressed; its `gh api` probe returned no inline review comments, so no PR-thread walk was made. 25 total.

**Process note.** The orchestrator ran no in-place falsification patches during the review window and the tree was verified clean while the lenses read it — the defect that corrupted two lenses' evidence in the second pass did not recur.

**Actioned (>=80): none.** The highest-scoring finding is 55, so nothing meets the actioned threshold and no finding reaches the return floor. Three claims of acceptance-criterion failure were each judged by the scorer to falsify the criterion only OUTSIDE the domain of the procedure it names, with the work itself correct — a criterion-wording question, not a defect:
- **O1 (40, AC1)** — `.Platform$OS.type` appears inside `fake_sys_which_path()`. Verified: it is the unix branch's host-capability escape hatch, not a re-decision of the simulated platform, which AC1's clause is about.
- **O5 (35, AC5)** — `check_*()` wraps its call in `try(silent = TRUE)` (`R/programs_check.R:19,44,70,96`) and `dir_walk()` in `tryCatch` (`R/utils.R:118`), so a swallowed refusal would not fail those callers' tests. Confirmed, but the swallowing is pre-existing `R/` code, which this milestone's Scope puts explicitly Out; the harness-side mechanism AC5 names raises on every routed call and a test asserts it fires.
- **O2 (20, AC2) / O3 (40, AC2)** — the unix-on-Windows-host degradation and `tools::file_ext()`'s alphanumeric-only match. AC2 itself states the Unix drive skips on a Windows host, and every extension the probe measured is alphanumeric.

**Logged, below the 80 threshold (25 findings), surfaced not dropped.**
O16 (55) `D-013`'s Consequences paragraph reads in the present tense about machinery T14 removed (`OPENAC_FULL_SUITE`, a `test_that` shadow, `Config/testthat/parallel`, a bare-`test_that()` rule) — none of it exists in the merged tree; it is M10's to land · O18 (50) the recorded AC1 amendment's trailing sentence was never appended to AC1's Evidence clause, though its substance sits in AC1's main clause and both halves were evidenced above; its other sub-claims target append-only history · O9 (45) the `resolve` fast path bypasses the shared predicate, so namer/predicate agreement is structural rather than observed · O3 (40) `tools::file_ext()` matches alphanumeric extensions only · O1 (40) discussed above · O10 (40) the AC4 walk reads `body()` but not `formals()` and requires the `rappdirs::` prefix · O5 (35) discussed above · O12 (35) `boundary_argv()`'s `as.character()` collapses `NULL` and `character(0)` · O4 (30) the sibling search is case-sensitive where real Windows is not · O11 (30) `local_fake_config()` after `local_fake_tools()` leaves `state$config` stale · O13 (30) `fake_program_name()` strips a superset of what the namer adds · O7 (25) `character(0)`/`NA` commands error as R internals rather than boundary violations · O15 (25) `local_fake_tools()` pins `os` at install, documented and required so the tree and its resolver cannot disagree · O2 (20) discussed above · O19 (15) AC1's "single call" read pedantically · O8 (15) `~` and `C:x` outside AC5's three forms · O6 (10) the check-before-record order AC5 requires · O14 (10) a comment detached from the function it documents · O20 (10) `NA_character_` reads as not-executable · B1 (10) the revert restores main's vacuity hole — declared in the commit, T14, D-013 and the ROADMAP edge to M10 · O17 (5) · B2 (5) · B3 (5) · B4 (5) · B5 (5), all explicit negative results.

**Gate outcome: PASSED.** No finding reached the actioned threshold, so no criterion failure and no amendment return. Defect returns for this milestone stand at 3, unchanged by this pass.

## Independent review (2026-08-08, second pass)

Three fresh-context lenses; findings scored by a fourth agent that generated none of them.
Diff-bug lens: 21. Blame-history lens: 3. Prior-PR-comments lens: 1 (its `gh api` probe found no real
inline review comments, so no PR-thread walk was made). 25 total.

**Evidence integrity.** Two lenses (P1, B1) and one blame finding (B2) were refuted before scoring.
P1/B1 both reported a `keep.source` dependency making the harness's file recording flaky; measured,
`keep.source` is FALSE during the test run in both invocation paths and the mechanism works anyway,
and 8 consecutive plain-`Rscript` runs on a verified-clean tree were 8/8 green. Their observed
failures were caused by the orchestrator's own in-place falsification patches, applied to the shared
working tree while they were reading it — a process defect recorded in the work log. B2 claimed the
`M07 B1/P1` citation is unsupported; M07's review recorded both (scores 35 and 33) in its full text
at commit ccc47ad, which the compressed archive drops.

**Actioned (>=80) — four findings, three of them acceptance-criterion failures.**

- **O6 (92, AC-FAIL AC8)** — "The completeness gate is disarmed by any *textual* mention of
  `local_fake_tools(`, including a comment. `harness_test_files()` greps every `test-*.R` for the
  literal string; any file in `expected` but not in `ran` turns the whole contract into a silent
  skip." Reproduced: appending `# see local_fake_tools() for why` to `test-installers.R` makes a full
  `devtools::test()` print "7 of 8 harness files ran; missing test-installers.R" and stop enforcing.
- **O15 (93, DEFECT)** — "`os` is a cached promise in `fake_sys_which()`, so `local_fake_os()` after
  first use is silently ignored." Reproduced: a closure created before the OS is faked keeps the old
  platform; a freshly created one picks up the new. `local_fake_downloads()`'s docstring claims it
  reads the platform `local_fake_os()` names and documents no ordering constraint.
- **O7 (90, AC-FAIL AC8)** — "Same disarm via a conditionally-skipped install. If every
  `local_fake_tools()` call in one file sits behind a platform/optional-package skip, that file is
  permanently `missing` on that platform and the gate skips there." Reproduced: "6 of 7 harness files
  ran". `test-whisper-transcribe.R` depends on `audio.whisper`, a GitHub-only Suggests.
- **O4 (85, AC-WRONG AC1)** — "AC1's evidence grep does not test AC1's proposition. After the return
  work the actual executability rule lives in `fake_sys_which_path()` (line 198), which the grep does
  not count, while `fake_sys_which()` is a mock factory, not a predicate. So the '2' is coincidental."
  Confirmed. A second genuine predicate could be added and the evidence would still pass.

**Logged, below the 80 threshold (21 findings), surfaced not dropped.**
O9 (72) the `harness_files()` test cannot fail from its own call — 8 earlier installs in the same file
already recorded the name · O14 (52) the `resolve` fast path bypasses the shared predicate entirely,
so namer/predicate agreement is structural rather than verified · O11 (50) `boundary_argv()`'s
`as.character()` collapses `NULL` and `character(0)` · O8 (45) D-010 not amended for the widened skip
surface · O21 (42) the `os =` override is documented but untested · O13 (40) `NA_character_` resolves
as "not executable" rather than erroring · O16 (40) `local_fake_tools()` can override a caller's own
`local_fake_config()`, whichever runs last winning · O1 (35) the sibling search is case-sensitive
where real Windows is not · O2 (35) `fake_program_name()` strips a superset of what the namer adds ·
O12 (35) zero-length command errors before the diagnostic fires · O17 (35) `harness_caller_file()`
can error on an edge-case `getSrcFilename()` shape · O18 (35) the O3 regression test cannot
discriminate on a Windows host · O3 (30) one extension list, two case semantics · O5 (25) the work
log cites helper lines 184/230; actual 189/235 · O19 (25) nothing asserts a legitimate absolute
command passes · O20 (20) the resolve branch builds a bindir path without an existence check ·
O10 (8) refuted, AC8's fail case was re-measured after the restructure · B1 (5) · B2 (5) · B3 (5,
self-declared non-defect) · P1 (5).

**Gate outcome: RETURNED.** O6 and O7 demonstrate AC8 failing inside its own domain, which is the
return floor; O4 additionally needs a gated amendment to AC1's evidence. Status -> in-progress.
Second defect return for this milestone. Thrash rule (b) fires — AC8 has failed twice, each time
because its completeness signal is a proxy that does not mean what the gate takes it to mean — and
the plan gate recorded no alternative for this mechanism, so `/milestone-brief` escalation is offered
per D-004. Trigger (a) does not fire at two returns.

## Independent review (2026-08-07)

Three fresh-context lenses; findings scored by a fourth agent that generated none of them.
Prior-PR-comments lens: **no findings** — it checked each of the nine absorbed findings and
confirmed all nine are addressed rather than reverted. Diff-bug lens: 24. Blame-history lens: 7.
31 total, deduplicated to 28 distinct.

**Actioned (>=80) — five findings, three of them acceptance-criterion failures.**

- **O1 (95, AC-FAIL AC2)** — "`<path>.exe` sibling branch unreachable. The early guard
  `!file.exists(path)` returns FALSE before the Windows branch, so the sibling check only fires
  when the extensionless path ALSO exists. With only `SMILExtract.exe` present,
  `fake_is_executable(".../SMILExtract", os = "Windows")` returns FALSE. AC2 says it must return TRUE."
  Reproduced directly. AC2's sibling clause was written from an inference, not from the probe —
  the probe created both `tool` and `tool.exe`, so the only-`.exe`-exists case was never measured.
- **O15 (93, AC-FAIL AC8)** — "The new runs gate turns filtered suite runs into failures.
  `devtools::test(filter = \"helper-boundary|zzz\")` gives FAIL 1 on a healthy tree." Reproduced.
  The gate conflates "the harness ran" with "the harness ran completely"; AC8 tested only the two
  extremes, full suite and single file.
- **O9 (85, AC-WRONG AC2)** — "AC2's text says 'Both drives run wherever the suite runs' but the
  Unix drive is skipped wholesale on Windows. Criterion ticked against a claim the code
  contradicts." The `skip_on_os("windows")` narrowing was recorded in the work log as a minor
  amendment; it changes what the criterion asserts and needed a gated amendment.
- **O3 (80, AC-FAIL AC1)** — "`fake_program_file()` reads HOST `.Platform$OS.type` while
  `fake_is_executable()` reads SIMULATED os. Under `local_fake_os(\"Windows\")` on macOS, bindir
  serves extensionless while the predicate refuses it." Unexercised today (no test combines the
  two helpers), but AC1 requires the platform their test's `local_fake_os()` names, not the host's.
- **O24 (80, DEFECT)** — "Scope 'In:' understates the blast radius: the diff also rewrites
  `test-programs-resolve.R` (34 lines) and `test-commands-probe.R`."

**Logged, below the 80 threshold (23 findings), surfaced not dropped.**
O2 (78) sibling rule returns the extensionless name, not the sibling — dead code while O1 stands ·
O4/B1 (78) `local_fake_downloads()` docstring still claims it resolves any existing file ·
O5 (72) recorder comment still names `normalizePath`, the mechanism AC5 rejected ·
O23/B7 (72) `local_fake_config()` still standalone, so a stray call makes a second disconnected dir ·
O8 (65) unix-on-Windows-host branch has zero coverage anywhere ·
O21 (62) nothing asserts a legitimate absolute command passes ·
O10 (62) the alias test's "every binding" loop issues the same call twice ·
O12 (55) rappdirs walk misses `formals()`, bare imports, `site_*_dir`, non-toplevel functions ·
O14 (55) the rappdirs assertion proves "differs from real", not "inside the sandbox" ·
O18 (55) queue-exhausted message uses the unstripped basename ·
B3 (55) D-010 not amended for the skip-semantics change ·
B6 (55) work log cites D-051, which is plugin doctrine, not in this repo's DECISIONS.md ·
O22 (45) the shared-rule test is largely tautological · O13 (45) self-contradicting comment ·
O16/B4 (45) `runs` incremented only by `local_fake_tools()` · O11 (40) the alias table records
behavior, not correctness · O6 (35) `~` and drive-relative forms outside AC5's three ·
O20 (30) `character(0)`/`NA` command edge cases · O7 (30) lowercase `"windows"` falls to the unix
branch · O17 (30) `harness_runs()` comment says boolean, returns count · B5 (30) the M07 citation
is an analogy, not the same code path · O19 (30) unconditional extension strip — AC3 called for it ·
B2 (25) `.exe`-only sibling matches AC2's literal text.

**Gate outcome: RETURNED.** Three actioned findings demonstrate acceptance criteria failing inside
their own domains (AC2 via O1, AC8 via O15, AC1 via O3), which is the return floor. AC2
additionally needs a gated text amendment (O9). Status -> in-progress. First defect return for this
milestone; the thrash rule's triggers do not fire.

