# RB02: How should the command-contract gate decide a test run was complete? (M09)

- **Date:** 2026-08-08
- **Output required:** write findings to `cairn/reviews/RR02-coverage-gate-run-scope.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`openac` is an R package that wraps four external command-line tools (FFmpeg,
FFprobe, OpenFace, openSMILE) plus a Whisper transcriber. Every wrapper reaches
its tool through a single chokepoint, `base::system2()`.

Decision **D-010** (`cairn/DECISIONS.md`) established a *command contract*: every
openac function from which `system2()` is transitively reachable must have a test
asserting the command it builds. The domain is **computed** from the call graph,
never hand-listed, and the contract is enforced by a **failing test** — D-010
explicitly rejected "an advisory-only report (a gap that never fails is a gap
that never closes)".

The mechanism: a test harness (`tests/testthat/helper-openac.R`) mocks
`base::system2`. Each mocked call records which openac function was on the
outermost stack frame, accumulating into a suite-wide registry. A test file named
to sort last, `tests/testthat/test-zzz-command-contract.R`, then compares the
computed domain against the set of functions actually recorded, and fails naming
any function that has no command test.

**The problem this brief is about.** That comparison is only meaningful when the
*whole* suite ran. Running a single file, or a filtered subset, leaves most of the
domain unrecorded through no fault of the code. So the gate needs a precondition:
*was this run complete?* Getting that precondition wrong is dangerous in two
opposite directions — too strict and every partial run reports dozens of false
failures; too loose and the gate silently stops enforcing, which is precisely the
vacuity D-010 forbids.

Milestone M09 has now tried twice and failed at review both times, each time
because the chosen signal did not actually mean "the run was complete":

- **Attempt 1** counted *harness installations* (`openac_registry$runs`). Review
  found this cannot distinguish a filtered run from a complete one, so
  `devtools::test(filter = "helper-boundary|zzz")` reported 21 functions as
  uncovered on a perfectly healthy tree.
- **Attempt 2** (current code) records *which test files* installed the harness,
  and compares that against the set of test files on disk whose **text** contains
  `local_fake_tools(`. Review found two holes, both reproduced:
  - Adding the string `local_fake_tools(` to any test file — **including inside a
    comment** — adds a file to the expected set that can never join the ran set,
    so the gate skips silently and permanently on every subsequent run.
  - A file whose only harness calls sit behind a conditional skip is expected but
    never ran, so the gate skips on that platform forever.
    `tests/testthat/test-whisper-transcribe.R` depends on `audio.whisper`, a
    GitHub-only `Suggests`, so this will fire on any machine lacking it.

Two failures of the same shape — a proxy signal mistaken for the thing it proxies
— is why this is being escalated rather than guessed at a third time.

## Materials

Read these files at the current branch `m09-harness-hardening`:

- `tests/testthat/test-zzz-command-contract.R` — the gate. `harness_test_files()`
  (~lines 79–93) computes the expected file set; the skip/enforce sequence is in
  the `test_that("every tool-calling function has a command test", ...)` block
  (~lines 95–160). `system2_closure()` computes the domain.
- `tests/testthat/helper-openac.R` — the harness. Relevant: the
  `openac_registry` environment and its `owners` / `runs` / `files` fields near
  the top; `harness_caller_file()`, which reads the calling test file off the call
  stack's srcrefs; `registered_owners()`, `harness_runs()`, `harness_files()`;
  and `local_fake_tools()`, which installs the mocks and does the recording.
- `cairn/DECISIONS.md` — D-010 in full.
- `cairn/milestones/M09-harness-hardening.md` — AC8 (the criterion at issue), the
  work log, and both `## Independent review` sections.
- `cairn/PROFILE.md` — the `test-doctrine` slot, for the repo's testing
  conventions.

To run things:

- Full suite: `Rscript -e 'devtools::test()'` (expect 504 pass / 0 fail, 2 skips).
- Filtered: `Rscript -e 'devtools::test(filter = "helper-boundary|zzz")'`.
- Single file: `Rscript -e 'devtools::load_all(quiet=TRUE); setwd("tests/testthat"); testthat::test_file("test-zzz-command-contract.R")'`.
- Package check (this is how CI runs the suite):
  `Rscript -e 'devtools::check()'`.

Two measured facts you can rely on, both verified on this machine:

- `getOption("keep.source")` is `FALSE` during the test run under both
  `devtools::test()` and the `R CMD check` / `test_check()` path, yet
  `harness_caller_file()` still returns the correct file — testthat attaches
  srcrefs through its own `srcfile` machinery, not through that option.
- `testthat:::find_test_scripts(path, filter = NULL)` returns the unfiltered test
  file list, but it is an unexported internal.

## Questions

1. **Is "was the run complete?" the right precondition at all?** The gate
   currently asks a whole-suite question from inside one test file. Is there a
   formulation that avoids needing to know the run's scope — for example,
   enforcing per-function against only the functions whose owning test file
   actually ran, or moving the contract out of the suite into a separate check?
   If such a formulation exists, prefer it and say why; if not, say so explicitly
   and answer question 2.

2. **If completeness must be detected, what signal should carry it?** Evaluate at
   least these, and any better option you see:
   (a) parsing each test file and walking the parse tree for a `local_fake_tools`
   symbol, rather than searching its text — this provably ignores comments and
   yields exactly the seven real harness files today, but does not address the
   conditional-skip hole;
   (b) recording that a test file was *sourced* rather than that it installed the
   harness, which would close the conditional-skip hole — is there a way to
   observe file sourcing from a testthat helper without editing every test file?
   (c) an explicit opt-in signal set by the runner (environment variable or
   option) declaring "this is a full run", set in `tests/testthat.R` and by CI;
   (d) using `testthat:::find_test_scripts()` or another testthat internal to
   learn the run's intended file set.
   For each: does it fail open or closed, what silently disarms it, and what does
   it cost a contributor who adds a new test file?

3. **What should the gate do when it cannot establish completeness?** Skip, fail,
   or warn? D-010 rejects advisory-only outcomes, but a partial run is not a
   contract violation. Is "skip" the right answer at all, given that a skip is
   exactly how both previous attempts failed silently? Consider whether the
   gate's *own* health should be separately asserted by a test that fails when
   the gate has stopped enforcing.

4. **How should this be tested so a third failure of the same shape is caught
   before review?** Both previous mechanisms passed their own tests. What
   assertion would have failed on attempt 2's comment hole? Name concrete tests.

5. **Is any part of the current harness's recording machinery
   (`harness_caller_file()`'s srcref walk, the three-field registry) worth keeping
   under your recommendation, or should it be removed?** Dead recording
   machinery is a maintenance cost and a false signal of rigor.

## Constraints

- **D-010 is binding and must not be relitigated**: the domain stays computed
  from the call graph, never a hand-maintained list, and coverage is enforced by
  a *failing* test, not an advisory report. If you believe D-010 itself is wrong,
  say so explicitly as a finding — do not quietly design around it.
- **M09 is test-code only.** Changes under `R/` are out of scope; if the right
  answer requires touching package code, say so explicitly rather than
  smuggling it in.
- `covr` percentages are a diagnostic, never a gate (`cairn/PROFILE.md`,
  `test-doctrine`). Do not propose a coverage-percentage threshold.
- The suite must stay green under `R CMD check` on all five CI platforms (macOS,
  Windows, Ubuntu devel/release/oldrel-1), where the source tree is absent and
  only the installed package plus `tests/` exist.
- Prefer solutions that do not depend on unexported testthat internals; if you
  recommend one anyway, say what breaks when testthat changes and how that
  failure would surface.

## Output format

In `RR02-coverage-gate-run-scope.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond the
brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Where findings bind implementation, also emit a
`## Binding criteria` section: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its tolerance.
These are ingested VERBATIM into M09's acceptance criteria and mechanically
diffed against this file; departures are legal only through that milestone's
shown "Deviations from RR02" table.
