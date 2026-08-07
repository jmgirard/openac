# M06: Wrapper testing contract — system2-boundary command tests

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP7, IP1
- **Branch/PR:** m06-wrapper-command-tests

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

- [ ] AC1 `tests/testthat/helper-openac.R` mocks `base::system2` via
      `testthat::local_mocked_bindings(.package = "base")` installed in the
      calling test's frame; records each call as `(command, args)` plus the
      call stack filtered to `asNamespace("openac")`; serves fake results from
      a queue that **errors** when exhausted. It also fakes tool resolution
      deterministically (an executable mode-0755 temp file via mocked
      `Sys.which()`), so results do not depend on what is installed on the
      machine. A test asserts interception through an alias (`ffm`), a primary
      name (`ffmpeg`), and an internal (`openac:::opensmile`).
- [ ] AC2 `tests/testthat/test-command-contract.R` computes its domain at test
      time: seeded at `base::system2`, it walks `body()` over every object in
      `asNamespace("openac")` — exported or not — matching **any symbol
      occurrence**, not only call heads, and takes the transitive closure. It
      subtracts a deferral list of **literal function names**, and fails if a
      deferred name is no longer in the computed closure (staleness). It fails,
      naming the function, if any remaining member records no `system2` call in
      the suite. Coverage is attributed to the **outermost** openac frame of
      each recorded call, never to a hand-maintained list of names. Test passes.
- [ ] AC3 For every function AC2's domain retains, the suite asserts the full
      ordered sequence of `(command, args)` pairs produced for default
      parameters. Each parameter is exercised once per distinct command shape it
      produces (flag parameters in both states; open-valued parameters at
      default plus one non-default); the passthroughs and aliases need only the
      pass-through identity assertion. Every file the exercised path reads —
      inputs, the fake openSMILE install tree with `config/<name>.conf`, and
      tool outputs a post-step re-reads (`os_fix_csv`) — is created under a
      per-test temporary directory.
- [ ] AC4 `find_program()` warns and returns `NULL`, not an error, on both
      not-found paths (no config file; config file naming a location that no
      longer resolves), and returns an absolute path when the program resolves
      via `Sys.which()` or via the config file — the positive fakes being real
      mode-0755 files, since `Sys.which()` returns `""` for a non-executable
      one. `check_ffmpeg()`, `check_ffprobe()`, `check_openface()` and
      `check_opensmile()` each return `FALSE`, not an error, when their tool is
      absent. The roxygen `@return` for `find_program()` matches the
      implemented behavior. Config I/O is redirected to a per-test temporary
      directory by mocking `rappdirs::user_config_dir()`.
- [ ] AC5 `ffp_count_streams()` returns the documented `c(Video=, Audio=)`
      counts (via `expect_equal`, as it returns integers) for mocked ffprobe
      output covering video+audio, audio-only, video-only and no-stream inputs.
      `os_check_audio()` and `aw_check_audio()` each return the documented
      logical for a conforming input and a non-conforming input carrying at
      least three probe fields; `aw_check_audio()` additionally returns `FALSE`
      for output with fewer than three fields.
- [ ] AC6 `devtools::document()` (roxygen2 8.0.0, matching `RoxygenNote`)
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
- [ ] T5 Command tests: `os_prep_audio`, `aw_prep_audio` — multi-call
      (ffprobe then ffmpeg), every command-affecting parameter per AC3's rule.
- [ ] T6 Command tests: `of_extract` (all 8 booleans), `os_extract`,
      `os_extract_wav` (needs the fake config tree and fake tool outputs).
- [ ] T7 Write `test-command-contract.R`: namespace symbol-occurrence closure
      from `system2`, literal deferral list + staleness assertion,
      outermost-frame coverage attribution, failure message naming gaps.
- [ ] T8 Run `document()`, `test()`, `check()`; record the NOTE baseline from
      `00check.log`; fix fallout.

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

## Decisions

## Review
