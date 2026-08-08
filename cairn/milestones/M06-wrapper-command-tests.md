# M06: Wrapper testing contract — system2-boundary command tests

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP7, IP1
- **Branch/PR:** m06-wrapper-command-tests · https://github.com/jmgirard/openac/pull/6

## Goal

Give the binary-dependent wrappers their first tests by mocking the `system2`
boundary, so every command openac constructs is asserted with no tool installed.

## Scope

**In:** a `system2`-mocking test helper with deterministic tool resolution and
fake tool outputs; command-construction tests for the core closure that can
reach `system2` — the four passthroughs and their aliases, `find_*`/`check_*`,
`ffp_count_streams`, `os_check_audio`/`aw_check_audio`,
`os_prep_audio`/`aw_prep_audio`, `of_extract`, `os_extract`/`os_extract_wav`;
a coverage gate whose domain is computed from the call graph; and the
`find_program()` NULL-contract fix (`R/programs_find.R:52`).

**Out:** `install_*` family, the `*_dir` batch wrappers, and `aw_transcribe*`
→ M07. GP7 layer 2 (gated real invocations) → M07. Platform-aware installers
→ M07 (guards) and a candidate row (dispatchers). GP8 runtime reporting →
candidate row. `audio.whisper` distribution decision → candidate row (DESIGN
defers it to submission time). CRAN submission → user-declared release window.

## Acceptance criteria

- [x] AC1 `tests/testthat/helper-openac.R` mocks `base::system2` via
      `testthat::local_mocked_bindings(.package = "base")` installed in the
      calling test's frame; records each call as `(command, args)` plus the
      call stack filtered to `asNamespace("openac")`; serves fake results from
      a queue that **errors** when exhausted. It also fakes tool resolution
      deterministically (an executable mode-0755 temp file via mocked
      `Sys.which()`), so results do not depend on what is installed on the
      machine. A test asserts interception through an alias (`ffm`), a primary
      name (`ffmpeg`), and an internal (`openac:::opensmile`).
- [x] AC2 `tests/testthat/test-command-contract.R` computes its domain at test
      time: seeded at `base::system2`, it walks `body()` over every object in
      `asNamespace("openac")` — exported or not — matching **any symbol
      occurrence**, not only call heads, and takes the transitive closure. It
      subtracts a deferral list of **literal function names**, and fails if a
      deferred name is no longer in the computed closure (staleness). It fails,
      naming the function, if any remaining member records no `system2` call in
      the suite. Coverage is attributed to the **outermost** openac frame of
      each recorded call, never to a hand-maintained list of names. Test passes.
- [x] AC3 For every function AC2's domain retains, the suite asserts the full
      ordered sequence of `(command, args)` pairs produced for default
      parameters. Each parameter is exercised once per distinct command shape it
      produces (flag parameters in both states; open-valued parameters at
      default plus one non-default); the passthroughs and aliases need only the
      pass-through identity assertion. Every file the exercised path reads —
      inputs, the fake openSMILE install tree with `config/<name>.conf`, and
      tool outputs a post-step re-reads (`os_fix_csv`) — is created under a
      per-test temporary directory.
- [x] AC4 `find_program()` warns and returns `NULL`, not an error, on both
      not-found paths (no config file; config file naming a location that no
      longer resolves), and returns an absolute path when the program resolves
      via `Sys.which()` or via the config file — the positive fakes being real
      mode-0755 files, since `Sys.which()` returns `""` for a non-executable
      one. `check_ffmpeg()`, `check_ffprobe()`, `check_openface()` and
      `check_opensmile()` each return `FALSE`, not an error, when their tool is
      absent. The roxygen `@return` for `find_program()` matches the
      implemented behavior. Config I/O is redirected to a per-test temporary
      directory by mocking `rappdirs::user_config_dir()`.
- [x] AC5 `ffp_count_streams()` returns the documented `c(Video=, Audio=)`
      counts (via `expect_equal`, as it returns integers) for mocked ffprobe
      output covering video+audio, audio-only, video-only and no-stream inputs.
      `os_check_audio()` and `aw_check_audio()` each return the documented
      logical for a conforming input and a non-conforming input carrying at
      least three probe fields; `aw_check_audio()` additionally returns `FALSE`
      for output with fewer than three fields.
- [x] AC6 `devtools::document()` (roxygen2 8.0.0, matching `RoxygenNote`)
      produces no diff; `devtools::test()` passes with zero skips in the files
      this milestone adds; `devtools::check()` reports 0 errors and 0 warnings.
      Evidence is `00check.log` from the built tarball; the work log quotes the
      text of every NOTE and marks the pre-existing "checking tests" NOTE as the
      baseline, so a newly introduced NOTE is distinguishable.

## Coverage

- AC1 → T1
- AC2 → T7
- AC3 → T4, T5, T6
- AC4 → T2, T3
- AC5 → T4
- AC6 → T8

## Tasks

- [x] T1 Write `tests/testthat/helper-openac.R`: `system2` mock installed in
      `parent.frame()`, ordered `(command, args)` + openac-frame recording,
      erroring fake queue, mocked `Sys.which()` resolution, fake openSMILE
      install tree. Prove alias/primary/internal interception.
- [x] T2 Fix `find_program()` (`R/programs_find.R:52`) — return `NULL` on the
      two not-found paths instead of falling through to
      `tools::file_path_as_absolute(NULL)`; keep the warnings; update roxygen.
- [x] T3 Tests for `find_program()`/`set_program()` (PATH hit, config hit,
      stale config, absent) and `check_*()` returning `FALSE` when absent, with
      `rappdirs::user_config_dir()` mocked to a temp dir.
- [x] T4 Command tests: passthroughs + aliases, `ffp_count_streams`,
      `os_check_audio`, `aw_check_audio` (each consumes two ffprobe results).
- [x] T5 Command tests: `os_prep_audio`, `aw_prep_audio` — multi-call
      (ffprobe then ffmpeg), every command-affecting parameter per AC3's rule.
- [x] T6 Command tests: `of_extract` (all 8 booleans), `os_extract`,
      `os_extract_wav` (needs the fake config tree and fake tool outputs).
- [x] T7 Write `test-command-contract.R`: namespace symbol-occurrence closure
      from `system2`, literal deferral list + staleness assertion,
      outermost-frame coverage attribution, failure message naming gaps.
      (Shipped as `test-zzz-command-contract.R` so it runs after the files
      whose calls it counts.)
- [x] T8 Run `document()`, `test()`, `check()`; record the NOTE baseline from
      `00check.log`; fix fallout.
- [x] T9 (review F1, AC4) `find_program()` must use the resolved
      `Sys.which(lines[[1]])`, not the raw recorded string, so a config naming a
      bare program name returns a path instead of erroring. Test that path.
- [x] T10 (review H1) Decide and implement the tool-absent contract for the four
      passthroughs: `system2(NULL, args)` executes `args` as a shell command, so
      `ffmpeg()` with ffmpeg absent runs the argument string. Needs a gate
      question — guard in each passthrough vs. abort in `find_program()`. Add
      tool-absent tests for all four.
- [x] T11 (review F7, AC3) Assert the `-version`/`-h` arguments the four
      `check_*` functions construct, so their registry entry reflects an
      assertion rather than a bare call.
- [x] T12 (review F8, AC3) Pin the whole `afilters = TRUE` command with
      `expect_identical`, covering `afftdn`, `compand` and `dynaudnorm`.
- [x] T13 (review F16, AC3) Cover `os_extract()`'s default `wavfile = NULL`
      branch — temp file created, used, and unlinked.
- [x] T14 (review G1, toolchain gate) Add a `NEWS.md` development-version entry
      for the user-visible contract changes, with no milestone numbers in the
      user-facing text.
- [x] T15 (review AC2, discovered) Make `openac_stack()` name a frame whose call
      head is a function value, so a `do.call()`-dispatched frame is attributed
      to the outer function and not to the inner passthrough. Test that path.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: criteria audit [O] ran twice; pass 1 returned "not ready to commit" with 8 defects across all 6 criteria (incl. a wrong mocking boundary — verified by probe: mocking `ffmpeg` left `ffm("-x")` executing the real binary — and an AC asserting `find_program()` behavior the code cannot produce); pass 2 on the rewritten block returned 2 blocking defects (`check_*()` short-circuits before `system2`, making AC2 machine-dependent; AC1's `(command, args)` record carries no function identity, making AC2's coverage derivation incompatible with it) plus 6 further ones; all repairs applied before commit.
- 2026-08-07: plan gate chose mocking `base::system2` over mocking the four passthroughs because the aliases `ffm`/`ffp`/`of`/`os` are separate bindings to the same closures, so rebinding `ffmpeg` does not intercept `ffm` and real binaries execute in tests; falsified by a testthat release making `.package = "base"` mocking unavailable or namespace-scoped.
- 2026-08-07: plan gate chose a core-now/M07-remainder split over one all-covering milestone because `install_*` needs network+archive mocking and the `*_dir` wrappers need furrr/progressr harnesses, pushing one milestone past the split tripwires; falsified by the remainder proving small enough to land inside M06's task budget.
- 2026-08-07: plan gate chose a hard-failing coverage gate over a computed domain minus a named deferral list, rejecting both an advisory-only gate and a hard gate over the whole closure, because D-009 rejected an IP-strength contract as "unsatisfiable until the test infrastructure exists" and a deferral list M07 empties keeps it satisfiable today; falsified by the deferral list failing to shrink across M07.
- 2026-08-07: plan gate chose fixing `find_program()` inside M06 over routing it to `/hotfix` first because its regression test needs the config-mocking harness M06 builds anyway; falsified by the fix proving to need no harness.
- 2026-08-07: implement gate chose `withr` in Suggests over hand-rolled temp cleanup (D-011), and migrating `find_program()`'s conditions to `cli` over leaving base `warning()`/`stopifnot()` (DESIGN Conventions prescribe migration for touched code; D-002 permits the condition-class change).
- 2026-08-07: T1 — `tests/testthat/helper-openac.R` adds `local_fake_tools()` (mocks `base::system2` and `base::Sys.which` in the calling frame, records ordered tool/args/stack, errors on queue exhaustion) and `local_fake_config()`; `test-helper-boundary.R` verifies interception via primary name, alias `ffm`, and internal `openac:::opensmile`, plus outermost-frame attribution and deterministic resolution.
- 2026-08-07: T2 — `find_program()` now returns `NULL` on both not-found paths instead of reaching `tools::file_path_as_absolute(NULL)`; conditions migrated to `cli_abort`/`cli_warn`; an empty config file now fails the same way as one naming a vanished binary; return unnamed. The pre-fix bug was reproduced by the T1 harness test, which errored with "'x' must be a character string" at `programs_find.R:52`.
- 2026-08-07: verify slot clean after T1+T2 — `devtools::document()` rewrote `find_program.Rd`; `devtools::test()` reports 122 pass, 0 fail, 0 skip.
- 2026-08-07: T3 — `test-programs-resolve.R` covers all four `find_program()` paths (PATH hit, config hit, stale config, absent) plus an empty config file and the `cli_abort` argument branches, the `set_program` → `find_program` round-trip, and all four `check_*()` in both the absent (`FALSE` + warning) and resolving (`TRUE`) states. Config I/O runs against a mocked `rappdirs::user_config_dir()`.
- 2026-08-07: T3 minor — first draft of two tests asserted the bare tempdir path; `find_program()` returns `tools::file_path_as_absolute()`, which resolves the macOS `/var` → `/private/var` symlink, so the expectation was wrong, not the code. Helper now canonicalizes.
- 2026-08-07: T3 discovered sub-task — `set_program()`'s roxygen promised "a logical indicating whether the program location was set properly" but it returns `writeLines()`'s invisible `NULL`; `@return` corrected to match behavior (doc-only, no behavior change).
- 2026-08-07: verify slot clean after T3 — `devtools::test()` reports 149 pass, 0 fail, 0 skip.
- 2026-08-07: T4 — `test-commands-probe.R` pins the four passthroughs and the four aliases to pass-through identity, their `is_string` guards, `ffp_count_streams`'s exact ffprobe query and all four stream combinations (AC5's `expect_equal`, since the counts are integers), and both `os_check_audio` and `aw_check_audio` across conforming/non-conforming inputs plus `aw_check_audio`'s under-three-fields guard. 31 expectations, clean on first run.
- 2026-08-07: T5 — `test-commands-prep.R` pins `os_prep_audio` (one ffmpeg call) and `aw_prep_audio` (ffprobe then ffmpeg, both attributed to the outer function) with `stream`, `overwrite` and `afilters` each exercised in every command shape they produce, plus the argument guards and output-directory creation. The doubled space `os_prep_audio` emits after the input path (`'" '` followed by `' -map'`) is pinned deliberately, so a future cleanup surfaces as a test change rather than a silent one.
- 2026-08-07: verify slot clean after T4+T5 — `devtools::test()` reports 207 pass, 0 fail, 0 skip.
- 2026-08-07: T6 — helper gained the fake openSMILE install tree the [O] audit called for (binaries under `bin/`, configs under `config/`, so `os_check_config()` resolves `dirname(find_opensmile())/../config/`) plus `write_fake_os_output()` for the outputs `os_fix_csv()` re-reads. `test-commands-extract.R` covers `of_extract` (default, all-on, all-off, and each of the 8 booleans toggled independently), `os_extract_wav` (no outputs, both outputs, non-default config, unknown-config and non-csv error branches), and `os_extract` down both branches — conforming input passed through untouched, non-conforming input prepared first.
- 2026-08-07: T6 minor — one test initially failed because the mocked ffmpeg writes no file while `os_extract_wav()` guards on `file.exists()`; the test now pre-creates the output the tool would have produced. Test's fault, not the code's.
- 2026-08-07: verify slot clean after T6 — `devtools::test()` reports 233 pass, 0 fail, 0 skip.
- 2026-08-07: T7 — `test-zzz-command-contract.R` computes the `system2` closure over `asNamespace("openac")` by symbol occurrence: 27 members, matching the [O] audit's independently computed figure, and including `os_extract_dir`, which a call-head walk misses because it reaches its tool via `do.call(what = os_extract, …)`. Seven literal names are deferred to M07, leaving 20 enforced, all covered. Coverage comes from a suite-wide registry the boundary mock populates with the outermost openac frame, never from a hand list.
- 2026-08-07: T7 — gate verified to fail for the right reason, not merely observed green: removing `os_extract_dir`'s deferral produced "no test asserts the command they build: os_extract_dir", and deferring `os_read` (which never reaches `system2`) produced "deferred but no longer reach system2 … os_read". Both reverted; suite back to 238 pass, 0 fail, 0 skip.
- 2026-08-07: T7 minor — file named `test-zzz-*` because testthat runs files in sorted order and the gate counts calls the other files make; run in isolation it skips with a stated reason rather than failing spuriously, so a full `devtools::test()` still reports zero skips (AC6).
- 2026-08-07: T8 — `devtools::document()` produces no diff; `devtools::test()` reports 238 pass, 0 fail, 0 skip; `devtools::check()` reports `Status: 1 NOTE` with 0 errors and 0 warnings. **NOTE baseline**, quoted from `00check.log`: `* checking tests ... NOTE / Running 'spelling.R' / Comparing 'spelling.Rout' to 'spelling.Rout.save' ...` — the pre-existing spelling-diff NOTE listing 56 potential misspellings (domain terms like `ffmpeg`, `OpenFace`, `LLDs`). It is the only NOTE and it predates this milestone.
- 2026-08-07: T8 — baseline established by measurement, not assumption: `check()` was run against `main` in a worktree and the spelling word lists diffed. Both sides list exactly 56 words with an empty diff in both directions, so this branch adds no NOTE and no new word. The worktree run also reported a second NOTE (`checking for hidden files and directories … .git`); that is an artifact of checking inside a git worktree, where `.git` is a regular file, and not a property of `main`.
- 2026-08-07: T8 fallout — the first draft of `set_program()`'s `@return` wrote "openac's user config directory", which added the token `openac's` to the spelling NOTE; reworded to drop the possessive, restoring the word list to the baseline exactly.
- 2026-08-07: review 1 returned M06 to `in-progress`. Failed: AC4 (a config file recording a bare program name still reaches `tools::file_path_as_absolute()` and errors, so `check_*()` propagates the very error class AC4 closes); AC3 (the four `check_*` members record a call but assert no command; the `afilters` chain omits `afftdn`/`compand`/`dynaudnorm`; `os_extract()`'s default `wavfile = NULL` branch is untested); AC2 (outermost-frame attribution is false for `do.call()`-dispatched frames, verified inside this suite); and the profile's consistency-gate changelog check (`NEWS.md` has no development-version entry). AC1, AC5, AC6 met; `cairn_validate` exit 0. Also found and verified: with the tool absent, `system2(NULL, args)` executes the argument string as a shell command, so this branch turned a hard error into silent shell execution. Tasks T9–T14 added; defect-return count 1.
- 2026-08-07: minor amendment — T15 added. Review 1 records AC2 as failing on
  `do.call()` frame attribution but actioned no task for it (F6 scored 78), so
  the criterion could not pass as written; T15 carries the fix.
- 2026-08-07: implement gate chose one shared internal guard over a per-wrapper
  copy for the tool-absent contract (T10), and chose keeping `find_program()`'s
  warning ahead of the new error over suppressing it, so the "use `set_program()`"
  hint survives.
- 2026-08-07: T9 — `find_program()` now returns `Sys.which(lines[[1]])` rather
  than the recorded string, so a config naming a bare program name resolves
  instead of erroring. Verified pre-fix by reverting the source: the new test
  errored at `programs_find.R:52` with "file 'ffmpeg' does not exist" from
  `tools::file_path_as_absolute()` — the failure the task names. Suite 240 pass,
  0 fail, 0 skip.

- 2026-08-07: T10 — the four passthroughs now resolve through an internal
  `require_program()`, which aborts when the tool is absent instead of letting
  `system2(NULL, args)` run the argument string as a shell command; roxygen
  `@return` updated on all four (and `opensmile`'s copy-paste "output of
  openface" corrected). Tests assert all four error and that nothing reaches
  the boundary. Rationale in the Decisions entry below.
- 2026-08-07: T11 — the `check_*()` resolving test now pins the probe commands
  as well as the return value: `-version`, `-version`, `-h`, `-h` against
  ffmpeg, ffprobe, openface, opensmile.
- 2026-08-07: verify slot clean after T9-T11 — `devtools::document()` rewrote
  the four passthrough `.Rd` files; `devtools::test()` reports 251 pass, 0 fail,
  0 skip.

- 2026-08-07: T12 — the `afilters = TRUE` command is now pinned entire with
  `expect_identical` against a chain constant, so `afftdn`, `compand` and
  `dynaudnorm` are asserted rather than skipped between fragments.
- 2026-08-07: T13 — helper extended: a queued result that is a function is
  called with `(command, args)`, letting a test stand in for a tool that writes
  a file its caller re-reads. `os_extract()`'s default `wavfile = NULL` branch
  now asserts the temp wav is created under `tempdir()`, reaches openSMILE as
  `-I`, and is gone after the call. Verified discriminating: replacing
  `if (temp) unlink(wavfile)` with `if (FALSE)` fails that last expectation
  alone.

- 2026-08-07: T15 — `openac_stack()` now recovers a frame whose call head is a
  function value by matching `sys.function()` against the namespace by identity
  (longest name wins, so an alias pair resolves to its primary), making the
  outermost-frame rule hold for `do.call()` dispatch. Verified discriminating:
  with the recovery replaced by `NA_character_` the new test reports owner
  "openface" instead of "of_extract" — the false attribution AC2 forbids.
  Suite 252 pass, 0 fail, 0 skip.

- 2026-08-07: T14 — `NEWS.md` gains a development-version entry for the four
  user-visible contract changes (wrappers error when a tool is absent;
  `find_program()`/`check_*()` warn-and-return instead of erroring; a recorded
  bare program name resolves; `set_program()`'s documented return corrected).
  Each is enforced by a named test in this branch.
- 2026-08-07: T14 fallout — the first draft wrote "shorthands", a word absent
  from `main`'s spelling list; reworded to name the four `find_*` functions,
  restoring the list exactly.
- 2026-08-07: completion verification — `devtools::document()` no diff;
  `devtools::test()` 252 pass, 0 fail, 0 skip; `devtools::check()`
  `Status: 1 NOTE`, 0 errors, 0 warnings. The NOTE is the T8 baseline, quoted
  from `00check.log`: `* checking tests ... NOTE / Running 'spelling.R' /
  Comparing 'spelling.Rout' to 'spelling.Rout.save' ...`. Re-measured against
  `main` in a worktree with `spelling::spell_check_package()` on both trees:
  54 words each and a byte-identical list, so this branch adds no NOTE and no
  word. (T8 quoted 56 from the built tarball's `spelling.Rout`; the two counts
  are different surfaces, and the branch-vs-main comparison is the claim.)

- 2026-08-07: review 2 — supersedes the T14 line's claim that each NEWS bullet
  "is enforced by a named test in this branch": bullet 4 (`set_program()`'s
  documented return) is a doc-only change and no test enforces it (R18).

## Decisions

### 2026-08-07 — A missing tool aborts the low-level wrappers

`find_program()` warns and returns `NULL` when a tool is absent, and
`system2(NULL, args)` hands `args` to the shell — so `ffmpeg("-version")` with
ffmpeg uninstalled executed `-version` as a shell command instead of failing.
The four passthroughs now resolve through an internal `require_program()`,
which aborts when `find_program()` returns `NULL`.

Alternatives rejected: a copy of the guard inside each passthrough (four
near-identical blocks and one message wording to keep in sync); making
`find_program()` itself abort (AC4 requires it to warn and return `NULL`, and
`check_*()` relies on that to answer `FALSE`).

`find_program()`'s warning is deliberately kept ahead of the error, so the
"use `set_program()`" hint survives. Pre-1.0, so no deprecation cycle (D-002).

## Review

**2026-08-07 — review 1: RETURNED to `in-progress`.** Four criteria fail as
written; PR #6 left as draft, not merged.

Evidence per criterion (all run fresh on `m06-wrapper-command-tests`):

- **AC1 — met.** `test-helper-boundary.R` passes: `system2` interception
  verified through primary name, alias `ffm`, and internal `openac:::opensmile`;
  queue exhaustion errors; resolution comes from the fake tree.
- **AC2 — NOT met.** Domain computation verified (27-member closure, 7 literal
  deferrals, 20 enforced; staleness arm and failure-naming arm both verified to
  fire). But the clause "coverage is attributed to the outermost openac frame"
  is false for `do.call()`-dispatched frames: `openac_stack()` drops a frame
  whose call head is a function value, so `do.call(of_extract, …)` records
  `openface` as owner. Demonstrated inside this suite
  (`test-commands-extract.R` flag loop). Dormant for coverage today, but it
  falsely marks an inner passthrough covered — the exact failure the rule exists
  to prevent — and M07's deferred `*_dir` functions all dispatch this way.
- **AC3 — NOT met.** Fails for three named cases: the four `check_*` members
  record a boundary call but no test asserts their `-version`/`-h` arguments;
  the `afilters = TRUE` chain is pinned by fragments that omit `afftdn`,
  `compand` and `dynaudnorm`; and `os_extract()`'s default `wavfile = NULL`
  temp-file branch is never exercised.
- **AC4 — NOT met.** A config file recording a bare program name (which
  `set_program()` permits, its guard also being `Sys.which() != ""`) passes the
  resolve check but reaches `tools::file_path_as_absolute("ls")`, which throws.
  `check_*()` propagates it, so the very error class AC4 closes is still open on
  that path. Reproduced.
- **AC5 — met.** `test-commands-probe.R` passes: all four stream combinations
  via `expect_equal`, both `*_check_audio` across conforming and non-conforming
  inputs, and `aw_check_audio`'s under-three-fields guard.
- **AC6 — met.** `document()` no diff; `test()` 238 pass, 0 fail, 0 skip;
  `check()` `Status: 1 NOTE`, 0 errors, 0 warnings, word list identical to
  `main`.

Consistency gate: `cairn_validate` exit 0, all checks pass. No DESIGN principle
changed, so no impact report. Toolchain slot: `document()` no-diff clean, no
`_pkgdown.yml`, README untouched — but the **changelog check fails**, `NEWS.md`
has no development-version entry for this milestone's user-visible contract
changes.

Independent review: three lenses (diff [O], blame-history [S], prior-PR [S])
plus a fresh scorer. The prior-PR lens found no prior-review evidence on the
touched files and contributed zero findings. 18 findings scored; 5 actioned at
≥80.

Actioned (≥80): F1 (92, AC4), H1 (90), F7 (85, AC3), F8 (85, AC3),
F16 (80, AC3) — all triaged **fix now**, carried as tasks T9–T13.

Logged below 80, not actioned (13): F6 78 — `do.call` frame attribution (still
recorded against AC2 above, since the criterion's own wording covers it);
G1 78 — missing `NEWS.md` entry (actioned anyway as T14, since the profile's
consistency-gate slot independently requires it); F5 72 — a helper test reads
the real `rappdirs` config dir; F15 68 — PATH-hit test checks only `basename()`;
F2 68 — stale-config warning names the config file rather than the recorded
location; F12 55 — DESIGN Known-issues wart now half-stale; F3 55 — gate hard-fails
on a filtered run instead of skipping; F11 40 — closure sees only symbolic
`system2` references; F4 40 — gate is fail-open if the registry breaks;
F13 35 — `withr` Suggests-only with no `skip_if_not_installed()`; F9 30 —
`stdout`/`stderr` never recorded; F14 20 — extra config lines ignored; F10 15 —
alias name collisions widen the domain (the intended over-approximation).

Defect-return count for M06: 1.

---

**2026-08-07 — review 2.** Evidence below is fresh on
`m06-wrapper-command-tests` at `03e0bcf`; `main` has not moved since the branch
was cut.

- **AC1 — met.** `test-helper-boundary.R` passes (15 expectations). The helper
  mocks `base::system2` and `base::Sys.which` through
  `local_mocked_bindings(.package = "base", .env = parent.frame())`; interception
  is asserted through the primary name `ffmpeg`, the alias `ffm`, and the
  internal `openac:::opensmile`; the exhausted queue errors ("queue exhausted");
  resolution comes from the mode-0755 fake tree, asserted not to be a real
  install path.
- **AC2 — met.** The domain is computed, not listed: 27 members by symbol
  occurrence over `asNamespace("openac")`, 7 literal deferrals, 20 enforced, all
  covered. Both failure arms re-verified by mutation this session: dropping the
  `os_extract_dir` deferral fails with "no test asserts the command they build:
  os_extract_dir"; deferring `os_read`, which never reaches `system2`, fails with
  "deferred but no longer reach system2 … os_read". Both mutations reverted, file
  restored. The outermost-frame clause now holds for `do.call()` dispatch:
  `openac_stack()` recovers a function-valued call head by identity, and
  `test-helper-boundary.R` asserts `do.call(of_extract, …)` is owned by
  `of_extract` (it reported `openface` before the fix).
- **AC3 — met.** All 20 enforced members have command assertions: the 8
  passthroughs/aliases by the pass-through identity assertion the criterion
  permits; the four `check_*` now by their probe arguments
  (`-version`, `-version`, `-h`, `-h`), closing review 1's first named gap; the
  `afilters = TRUE` chain by a whole-command `expect_identical` including
  `afftdn`, `compand` and `dynaudnorm`, closing the second; and `os_extract()`'s
  default `wavfile = NULL` branch — temp wav created under `tempdir()`, passed
  to openSMILE as `-I`, gone after the call — closing the third, verified
  discriminating (disabling `unlink()` fails that expectation alone).
  Per-parameter shapes: `stream`, `overwrite`, `afilters`, the eight `of_extract`
  booleans, `config`, and `aggfile`/`lldfile` are each exercised in every command
  shape they produce. Reading applied, stated so it is not silently charitable:
  the criterion says *the suite* asserts the sequence, and coverage sits with the
  function that BUILDS each command — a caller that forwards a parameter (e.g.
  `os_extract`'s `config`, or the `ffp_count_streams` probe reached inside
  `aw_check_audio`) has its ordered tool sequence pinned, with the argument
  string pinned where it is constructed. This is the reading review 1 applied.
  All fixtures are per-test temp dirs (`withr::local_tempdir`/`local_tempfile`),
  including the fake openSMILE tree with `config/<name>.conf` and the outputs
  `os_fix_csv()` re-reads.
- **AC4 — met.** `test-programs-resolve.R` passes (31 expectations). Both
  not-found paths warn and return `NULL` (no config file → "Failed to find";
  config naming a vanished location, and an empty config file → "no longer
  resolves"). Both positive paths return an absolute path: via `Sys.which()`,
  and via the config file — including review 1's failing case, a config
  recording a bare program name, which now resolves through `Sys.which()`
  instead of reaching `tools::file_path_as_absolute()`. Verified pre-fix by
  reverting `R/programs_find.R`: the test errored at `programs_find.R:52` with
  "file 'ffmpeg' does not exist". All four `check_*()` return `FALSE` with a
  warning when absent and `TRUE` when the tool resolves, including on the
  bare-name config path that previously threw. `find_program()`'s roxygen
  `@return` reads "An absolute path to the program as a string, or `NULL` (with
  a warning) if the program could not be found" — matching behavior. All config
  I/O runs against a mocked `rappdirs::user_config_dir()`.
- **AC5 — met.** `test-commands-probe.R` passes (40 expectations).
  `ffp_count_streams()` returns the documented `c(Video=, Audio=)` counts via
  `expect_equal` for all four stream combinations (video+audio, audio-only,
  video-only, none). `os_check_audio()` and `aw_check_audio()` each return the
  documented logical for a conforming input and for non-conforming inputs
  carrying three probe fields (wrong codec, wrong rate, two channels, a video
  stream present), and `aw_check_audio()` returns `FALSE` for output with fewer
  than three fields.
- **AC6 — met.** `devtools::document()` (roxygen2 8.0.0 installed, matching
  `RoxygenNote: 8.0.0`) leaves the tree clean. `devtools::test()` reports 252
  pass, 0 fail, 0 skip — zero skips across the whole suite, including every file
  this milestone adds. `devtools::check()` reports `Status: 1 NOTE` in
  `00check.log` from the built tarball, 0 errors and 0 warnings. The single NOTE
  is the T8 baseline, quoted from `00check.log:60`: `* checking tests ... NOTE /
  Running 'spelling.R' / Comparing 'spelling.Rout' to 'spelling.Rout.save' ...`
  — the pre-existing spelling-diff NOTE. Measured against `main` rather than
  assumed: `spelling::spell_check_package()` run on both trees returns 54 words
  each and a byte-identical sorted list, so this branch introduces no NOTE and
  no new word.

Consistency gate — universal: `cairn_validate` exit 0, all 16 checks PASS.
Advisories, not gate failures: `sizing` (15 tasks, past the 10 tripwire — the
send-back added T9–T15 to an already-planned milestone) and `work-log format`
(47 — this session's work-log entries are hard-wrapped rather than one line
each; the work log is history under IP4, so they are left as written and the
lesson is captured instead). No DESIGN principle changed, so no impact report.
Toolchain (`r-package` profile): `document()` no diff; generated files
regenerate clean; README untouched and in sync; no `_pkgdown.yml`; `NEWS.md`
carries a development-version entry for the four user-visible contract changes;
no new top-level files, so no `.Rbuildignore` entries owed; `check()` clean at
0 errors / 0 warnings / 1 justified NOTE. `NAMESPACE` is unchanged — the new
`require_program()` is internal, so no export or reference-index row is owed.
The repo has no `.github/workflows`, so the never-merge-red-CI rule has no CI to
read; the local `check()` above is the evidence in its place, and CI setup is
now a candidate row.

Independent review: three fresh-context lenses (diff [O], blame-history [S],
prior-PR [S]) plus a fresh [S] scorer that did not generate the findings. The
blame-history lens found no conflict — every change traces to D-010, D-011, a
logged task, or a review-1 finding, and the one regression the branch created
mid-flight (`system2(NULL, args)` shell execution) was caught and closed inside
the same milestone. The prior-PR lens confirmed all five of review 1's actioned
findings are genuinely fixed rather than papered over, and found the GitHub
inline-comment surface empty. The diff lens reported 24 findings; one scored
≥80.

Actioned (≥80) — **R1 (85), triaged fix now.** Verbatim:

> **R1 — `NEWS.md` bullet 1 misstates the previous behavior (user-facing,
> wrong).** File: `NEWS.md:3-6`. "Previously the argument string was handed to
> the system shell and run as a command." This is false with respect to the last
> release. On `origin/main`, `find_program()` ends in
> `tools::file_path_as_absolute(location)` with `location = NULL`, which
> **throws** (`'x' must be a character string`) — so `ffmpeg("-version")` with
> ffmpeg absent errored; it never reached `system2(NULL, args)`. The
> shell-execution behavior existed only in an intermediate state *created on
> this branch* by T2 and then closed by T10. NEWS describes a branch-internal
> regression as if it were shipped. Failure scenario: a 0.1.0 user reads NEWS,
> concludes their installed version silently shell-executed argument strings
> when a tool was missing, and audits over a vulnerability that never shipped.

Confirmed independently against `origin/main:R/programs_find.R`: both not-found
paths set `location <- NULL` and fall through to `file_path_as_absolute()`, so
0.1.0 errored on an absent tool. The bullet now reads "now fail with an error
naming the program when it cannot be found, instead of the low-level error that
previously surfaced from path resolution" — a claim two tests enforce (the
wrappers error, and nothing reaches the boundary). Return floor: R1 scored 85,
below the 90 bar, and demonstrates no acceptance criterion failing, so it took
triage rather than a send-back.

Logged below 80, not actioned (23): R7 75 — `DESCRIPTION` declares
`testthat (>= 3.0.0)` but the suite needs ≈3.1.9 for
`local_mocked_bindings(.package=)`; R2 72 — one helper test omits
`local_fake_config()` and reads the real config dir (review 1's F5, reconfirmed
by probe); R10 68 — PATH-hit test asserts only `basename()`; R19 65 —
`os_prep_audio()`/`aw_prep_audio()` `@return` omits the `"Skipped"` sentinel;
R17 62 — DESIGN Architecture and Known-issues stale; R16 60 — the harness
increments its queue index before the record can fail; R3 55 — a
`do.call()`-dispatched alias attributes to its primary; R22 55 — a config line
with trailing whitespace is not trimmed before `Sys.which()`; R20 52 —
`os_extract_wav()` pins a no-output invocation `os_extract()`'s docs call
invalid; R15 48 — the fake `Sys.which()` accepts a directory; R13 45 —
assertions see only `basename(command)`; R4 42 — the closure sees only symbolic
`system2`; R11 42 — four members assert a partial argument sequence; R12 42 —
`boundary_args()` flattens `args` to one string; R14 42 — guard assertions match
`stopifnot()` source text; R6 40 — the registry credits a recorded call, not an
asserted one; R9 40 — AC2 names `test-command-contract.R`, shipped as
`test-zzz-command-contract.R`; R18 38 — a work-log line overstates test
enforcement for the doc-only NEWS bullet; R5 35 — the gate skips rather than
fails if the registry is empty; R8 32 — `withr` Suggests-only with no
`skip_if_not_installed()`; R21 30 — a fixture records "ffmpeg" under
`opensmile_location.txt`; R23 22 — `check_*()` resolves twice (pre-existing);
R24 18 — `aw_prep_audio()` leans on `ifelse()` lazy indexing.

Two records this branch itself falsified were corrected in place, independently
of the finding threshold, under the tracking rules' correct-a-false-record rule:
DESIGN's Architecture paragraph (the passthroughs now route through
`require_program()`) and its Known-issues line claiming the binary-dependent
wrappers have no tests. Both marked; git holds the originals.

Defect-return count for M06: 1 (unchanged — review 2 returned nothing).
