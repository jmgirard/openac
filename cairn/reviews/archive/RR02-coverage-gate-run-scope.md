# RR02: How should the command-contract gate decide a test run was complete?

- **Date:** 2026-08-08
- **Brief:** `cairn/reviews/RB02-coverage-gate-run-scope.md`
- **Reviewed at:** branch `m09-harness-hardening`, working tree clean at review
  time.

**Evidence gathered for this report** (all on this machine, macOS,
`audio.whisper` NOT installed here — checked with
`requireNamespace("audio.whisper", quietly = TRUE)` → `FALSE`):

- Full suite `devtools::test()`: **FAIL 0, PASS 504, SKIP 2** (both
  `test-real-tools.R`), gate enforcing.
- Filtered `devtools::test(filter = "helper-boundary|zzz")`: FAIL 0, SKIP 1 —
  "1 of 7 harness files ran; missing …" as documented.
- A scoped experiment (scratch dir, four test files, `testthat::test_dir()`
  with `options(keep.source = FALSE)`) establishing the mechanism question 2(b)
  asks about; details under Q2(b).

---

## 1. Is "was the run complete?" the right precondition at all?

**Yes — completeness is inherent to what the contract asserts, and no
formulation escapes it.** The contract is a *global absence* claim: "no
function in the domain lacks a command test **anywhere in the suite**." A
partial run can witness presence (this function was covered) but can never
witness absence, because absence is only decidable over the whole suite. The
two suggested escapes both fail:

- **Per-function enforcement against only the functions whose owning test file
  ran** is impossible for exactly the functions the gate exists to catch. An
  *uncovered* function has no owning test file — that is what being uncovered
  means — so there is no "its file didn't run" excuse available to it, and no
  computable way to distinguish "its file didn't run" from "it has no file"
  without a function→file ownership map. Such a map cannot be computed from a
  partial run (the run never saw the function), and maintaining one by hand is
  the hand-listed domain D-010 forbids. Every road from this formulation leads
  back to either a hand map or a completeness check.
- **Moving the contract out of the suite** (a separate CI step or script that
  runs the suite itself and then checks) makes the contract advisory for
  everyone not running that step — locally it never fails, which is the
  "advisory-only report" D-010 explicitly rejected. Reject.

So the precondition stands. **But the two failures were not failures of the
precondition — they were failures of *inference*.** Both attempts tried to
infer completeness from a content-derived proxy (install counts; a text match
over file contents) and both proxies diverged from the thing proxied. The
correct move is not a third, cleverer inference: it is to make completeness an
**observation of execution** (which files actually executed, recorded at
execution time) compared against a **content-free ground truth** (which test
files exist on disk), with an **explicit declaration** from the runner
escalating "cannot establish completeness" from skip to fail where the run is
known to be full. Question 2 develops this.

One structural fact makes "which files executed" a *sound* completeness
signal rather than another proxy: testthat's `filter` selects **whole files**,
never individual tests. A run is therefore complete if and only if every
`test-*.R` file on disk executed. That biconditional is exact, not a proxy.

## 2. What signal should carry completeness?

### (a) Parse each test file for a `local_fake_tools` symbol

Parsing (`parse()` + walking for the symbol) is strictly better than the text
grep: it provably ignores comments and strings, so attempt 2's comment hole
closes, and it costs a contributor nothing. But it keeps the *shape* that
failed twice: "this file's source contains a call site" is still a proxy for
"this file will install the harness when run," and the two diverge whenever a
call site exists but does not execute — a conditional skip, a call site inside
a defined-but-uncalled local helper, dead code behind `if (FALSE)`. Each
divergence puts a file in `expected` that never joins `ran`, and the gate
**fails open** (silent, permanent skip — the exact disarm of attempts 1 and
2). Silent disarms: any conditionalized call site. Contributor cost: zero.
**Reject as the completeness signal** (it remains a fine technique if a
harness-file list is ever wanted for another purpose).

### (b) Record that a test file was *sourced* — yes, this is observable without editing any test file

This is the recommended signal, and the mechanism the brief asks after
exists: **shadow `test_that` in `helper-openac.R`.** testthat sources helpers
into the environment that parents every test file's execution environment, so
a helper-defined `test_that` shadows `testthat::test_that` for every bare
`test_that(...)` call in every test file — which is every test in this suite
(verified: no file uses a qualified `testthat::test_that()` or `describe()`).
The shadow records the executing file off its own call's srcref, then forwards
the *unevaluated* call:

```r
test_that <- function(desc, code) {
  cl <- sys.call()
  # record the file this call was sourced from (srcref), then forward
  openac_registry$sourced <- c(openac_registry$sourced, harness_caller_file())
  cl[[1]] <- quote(testthat::test_that)
  eval.parent(cl)
}
```

Measured in a scratch fixture suite under `testthat::test_dir()` with
`options(keep.source = FALSE)` — the same srcref regime the brief measured for
`harness_caller_file()`:

- a bare `test_that()` resolves to the shadow and the file name is recovered
  from the srcref (`keep.source` FALSE notwithstanding — testthat's own
  `srcfile` machinery, the same measured fact the brief records);
- a file whose only test body is `skip("...")` **is still recorded** — the
  shadow runs before the body does — closing the conditional-skip hole
  entirely;
- forwarding by call rewrite preserves lazy evaluation of `code`, skip
  semantics, and source locations (the reporter still printed
  `test-b-skips.R:2:3` for the skip);
- a **qualified** `testthat::test_that()` call bypasses the shadow (its file
  was not recorded). This is the one disarm vector; see below.

With this signal, `ran` = files observed executing, and `expected` = **every**
`^test-.*\.[Rr]$` file on disk (`list.files()`, no content read of any kind).
There is no proxy left: `expected` is ground truth by definition of the file
set, `ran` is an observation, and `filter`'s file granularity makes
`ran == expected` exactly equivalent to "the run was complete."

- **Fails open or closed?** Closed, given the canary in Q3: if testthat ever
  changes helper/test-file scoping so the shadow stops resolving, *no* file is
  recorded — including the contract file itself — and the canary
  ("this file is in `ran`") fails in every run mode, single-file included. A
  breakage cannot present as a skip.
- **What silently disarms it?** (i) A test file written with qualified
  `testthat::test_that()` calls or `describe()/it()` throughout — that file
  never joins `ran`, so the gate skips locally (message naming the file) and
  **fails** under the declared-full runs of (c) below. Noisy on CI, not
  silent. (ii) testthat parallel mode (`Config/testthat/parallel: true`, not
  set in this repo and absent today): each file would run in its own process
  and the registry would not accumulate — same failure surface as the current
  design; guard with a BC (BC9). (iii) A test file sorting *after*
  `test-zzz-command-contract.R` executes after the gate reads the registry;
  guarded by an in-file assertion that the gate file sorts last (BC8).
- **Contributor cost:** zero. A new test file is expected the moment it
  exists and records itself the moment it runs; nothing to register, no
  string to include, no list to edit.

**Apply.**

### (c) Explicit opt-in signal from the runner

As the *sole* signal, reject: `tests/testthat.R` is executed by
`R CMD check` but **not** by `devtools::test()`, so an env-var-only gate never
enforces in the contributor's primary loop — the gate would be CI-only, which
is the advisory outcome D-010 rejects, merely relocated. It fails open
locally (never enforces) and its silent disarm is trivial (unset variable).

As an **escalation layer over (b), apply**: `tests/testthat.R` (which is
`tests/`, hence inside M09's test-only scope) sets
`Sys.setenv(OPENAC_FULL_SUITE = "true")` before `test_check("openac")`. The
semantics: the variable never *enables* enforcement — (b) does that whenever
`ran == expected`, including plain local `devtools::test()` — it converts
"cannot establish completeness" from *skip* to *fail* on runs that are
declared full. Under `R CMD check` (all five CI platforms, and CRAN) a file
missing from `ran` is then a red build naming the file, never a silent skip.
Contributor cost: zero; the file is set-and-forget.

### (d) `testthat:::find_test_scripts()` or other internals

Reject. With `filter = NULL` it computes nothing `list.files()` does not; to
learn the *actual* run's scope you would need the live `filter` argument,
which is not exposed — you would be walking `sys.frames()` for
`test_dir`'s promise, an unexported call-stack shape. When testthat
reorganizes internals the walk returns nothing or the wrong thing, and the
gate would fail open (skip forever) with no signal that it had — the exact
silent-disarm class under review. The failure would surface, if ever, as "the
gate has not failed in a long time," which is no surface at all.

### The recommended composite

1. `ran`: recorded by the `test_that` shadow at execution time (b).
2. `expected`: `sort(list.files(test_path("."), pattern = "^test-.*\\.[Rr]$"))`
   — content-free, so the comment hole is structurally unexpressible.
3. Escalation: `OPENAC_FULL_SUITE` set in `tests/testthat.R` (c) turns
   incomplete-under-declaration into failure.
4. Canary + ordering guard (Q3) make the gate's own health a tested property.

## 3. What should the gate do when it cannot establish completeness?

**Three-way, by what is known about the run:**

- **Undeclared partial run** (local filtered/single-file work): **skip**, with
  a reason naming every missing file (the current message format is good).
  A partial run is legitimate developer workflow and not a contract
  violation; failing it would train contributors to ignore red, which is
  worse than skip.
- **Declared-full run with files missing from `ran`** (`OPENAC_FULL_SUITE`
  set): **fail**, naming the missing files. On a declared-full run, "a file
  did not execute" is itself a defect — a crashed file, a bypassed shadow, a
  misordered gate file — never a scoping choice.
- **Complete run** (`ran == expected`, declared or not): enforce; and keep the
  existing hard assertions that `covered` is non-empty (broken attribution
  must fail, as the current code already ensures).

**Is "skip" safe at all, given the history?** Only if the *reachability of the
enforce branch* is itself a tested property — both prior failures were skips
that nothing was watching. Two guards make skip safe:

- **The canary.** The contract file's first test asserts, in **every** run
  mode including `testthat::test_file()` on itself:
  `expect_true("test-zzz-command-contract.R" %in% harness_sourced_files())`.
  The gate file records itself through the same shadow every other file uses,
  so if the recording machinery breaks — the failure class that would make
  every future run skip — the very next run of any scope **fails**, loudly,
  before any skip logic is consulted. This is the "gate's own health
  separately asserted" test the question asks for, and it is what neither
  prior attempt had: their signal could break with no test watching the
  signal.
- **The escalation.** CI and `R CMD check` never skip-on-incomplete; they
  fail. A disarm that survives locally as a skip message cannot survive a
  single CI run.

D-010 is satisfied: on every complete run the contract is a failing test; the
only surviving skip is the genuinely partial local run, and that skip's
absence-of-enforcement is bounded by the next `R CMD check`.

## 4. How should this be tested so a third failure of the same shape is caught before review?

The root cause both times was that **signal collection, ground truth, and the
skip/fail decision were fused into one test body**, so the only way to test
the gate was to run whole suites in the right shapes — and nobody ran the
shape that broke. Separate them into pure functions and the dangerous cases
become one-line unit tests:

1. **Factor the decision into a pure function**, e.g.
   `contract_decision(expected, ran, covered, domain, deferred, declared_full)`
   returning one of `enforce_pass` / `enforce_fail(uncovered)` /
   `skip_partial(missing)` / `fail_incomplete(missing)` /
   `fail_broken_attribution`. Unit-test every branch with literal character
   vectors — no suite runs, no mocking. Attempt 1's hole is the test case
   `ran ⊂ expected, declared_full = FALSE → skip_partial` (it returned
   enforce_fail); attempt 2's holes are `expected` containing a name `ran`
   can never contain — which the next item makes unconstructible.
2. **Assert the expected-set is content-free.** Point the (dir-parameterized)
   expected-set function at a fixture directory containing: a test file whose
   only mention of `local_fake_tools(` is inside a comment; a test file with
   no harness reference at all; a file of unparseable garbage named
   `test-garbage.R`. Assert all three are in `expected` and — the assertion
   that **would have failed on attempt 2's comment hole** — that
   `expected` computed twice, once after appending
   `# local_fake_tools()` to any fixture file, is *identical*: content
   changes cannot move the set. (Under attempt 2 the append moves the set;
   the test fails naming it.)
3. **Self-hosted fixture-suite test for the `ran` signal.** Inside one test,
   run `testthat::test_dir()` on a scratch fixture suite (three files: one
   normal, one whose only test's first statement is `skip()`, one gate-like
   reader) using the same shadow helper, and assert the skipping file **is**
   in the recorded set. This is the assertion that would have failed on the
   conditional-skip hole in any install-recording design, and it passes only
   for execution-time recording. (My Q2(b) experiment is exactly this test,
   minus the `expect_`s.)
4. **The canary itself** (Q3) is a standing test that fails on any recording
   breakage in any run mode — it converts "the gate has quietly stopped
   enforcing" from an unobservable state into a first-run failure.
5. **Escalation test:** with `withr::local_envvar(OPENAC_FULL_SUITE = "true")`
   and a registry state missing one expected file, assert the decision is
   `fail_incomplete` naming the file (pure-function test; no suite run
   needed).
6. **Review protocol (mutation evidence), recorded in the work log:** the
   review re-runs attempt 2's two reproductions as *mutations that must now
   be harmless or loud* — (i) append a comment mentioning
   `local_fake_tools(` to a real test file, full suite must still enforce
   and pass; (ii) wrap one real harness file's tests behind a forced
   `skip()`, full suite must still record the file, establish completeness,
   and **fail** on the functions that file covered (uncovered), never skip.
   Mutation (ii)'s expected outcome is a *failure naming functions* — the
   loud direction — which is the acceptance shape AC8 should demand.

## 5. What recording machinery is worth keeping?

- **`openac_registry$owners` + `openac_stack()` + the recording inside
  `fake_system2()`: keep unchanged.** This is the coverage half of the gate
  (which functions the suite actually asserted), is orthogonal to the
  run-scope question, and survived both reviews intact.
- **`harness_caller_file()`: keep, rehomed.** Its srcref walk is measured to
  work under both invocation paths (brief's measured fact; re-confirmed in my
  experiment with `keep.source = FALSE`) and it is exactly what the
  `test_that` shadow needs to name the executing file. It becomes the
  shadow's file-identification routine.
- **`openac_registry$files` (install-site recording inside
  `local_fake_tools()`): remove the recording call; repurpose or rename the
  field for the shadow's sourced-file record.** "Which files installed the
  harness" answers no question the gate asks once completeness is
  execution-based, and keeping it invites the next reviewer to think it is
  load-bearing.
- **`openac_registry$runs` / `harness_runs()`: remove.** It exists only to
  distinguish "nothing installed" from "installed but unattributed," and both
  states are now decidable from `sourced` + `owners` (`sourced` empty →
  single-file/no-shadow run, caught by the canary; `sourced` complete with
  `owners` empty → broken attribution, the existing `expect_gt` failure). A
  counter that no assertion needs is the "false signal of rigor" the question
  names.
- **`harness_test_files()` and the assembled `local_fake_" "tools(` needle:
  delete entirely.** The text search is the defect under review, and the
  self-match-avoidance hack exists only to serve it. Nothing else calls it.

## Beyond the brief

- **The brief's concrete O7 instance is factually wrong, and the correction
  strengthens the design.** `test-whisper-transcribe.R` does **not** depend on
  `audio.whisper`: it mocks `predict` (openac imports `stats::predict`;
  `local_mocked_bindings()` rebinds it in openac's namespace) and its header
  comment says explicitly that audio.whisper need not be installed. Measured
  on this machine, which *lacks* audio.whisper: the full suite runs 504/0
  with all 7 harness files running and the gate enforcing. The
  conditional-skip *class* of hole is real (the review reproduced it with an
  artificial skip), but no instance exists in the suite today — and the
  whisper file is the existence proof of the doctrine that should replace
  accommodation: **because the boundary is fully mocked, a command test never
  has a legitimate reason to conditionally skip.** A command test that cannot
  run everywhere is a harness gap or an explicit `deferred` entry — so the
  right gate behavior on a complete run with a skipped-out command test is
  the loud one (fail naming the functions), which is exactly what the Q2
  design produces. M09's milestone file and AC8's rationale should drop the
  audio.whisper claim so it does not steer a future review.
- **Parallel testthat is a standing incompatibility** for any
  registry-accumulation design (attempts 1, 2, and this recommendation
  alike): under `Config/testthat/parallel: true` each file runs in its own
  process and no cross-file registry survives. Not set in this repo; make it
  a checked precondition (BC9) rather than an unstated assumption.
- **Gate-file ordering is an unstated assumption**: a future
  `test-zzz2-*.R` would execute after the gate and permanently sit in
  `missing`. One in-file assertion (gate file sorts last in `expected`)
  closes it (BC8).
- **`tests/testthat.R` carries a "do not modify" boilerplate comment**; the
  env-var line is a standard, documented use of that file (test
  configuration), but the comment block should be trimmed or annotated when
  the line is added so the next reader does not flag the edit as a violation.

## Recommendations

1. **Apply** — record `ran` by shadowing `test_that` in `helper-openac.R`
   (call-rewrite forwarding; file named via `harness_caller_file()`); this is
   question 2(b), verified feasible without editing any test file.
2. **Apply** — compute `expected` as all `^test-.*\.[Rr]$` files on disk with
   no read of file contents; delete `harness_test_files()` and its needle.
3. **Apply** — `Sys.setenv(OPENAC_FULL_SUITE = "true")` in
   `tests/testthat.R`; declared-full runs **fail** (never skip) when
   `ran != expected`, naming the missing files. Undeclared partial runs skip,
   naming the missing files.
4. **Apply** — the canary: the contract file asserts its own presence in
   `ran` in every run mode, so recording breakage fails the very next run of
   any scope.
5. **Apply** — factor the skip/fail/enforce decision into a pure function of
   `(expected, ran, covered, domain, deferred, declared_full)` and unit-test
   every branch, plus the fixture-dir tests in Q4 (2) and (3).
6. **Apply** — remove `openac_registry$runs`, `harness_runs()`, and the
   install-site file recording; keep `owners`, `openac_stack()`,
   `harness_caller_file()`.
7. **Apply** — review-time mutation protocol Q4 (6): comment-append must be
   harmless; forced-skip of a harness file must produce a *failure naming
   functions* on a full run.
8. **Consider** — a one-line note in D-010's consequences (or a new decision)
   recording that the contract's completeness precondition is
   execution-observed + runner-declared, so the next reviewer inherits the
   rationale; and correcting the audio.whisper claim in M09's log at the next
   gated amendment.
9. **Reject** — parse-tree detection of `local_fake_tools` call sites as the
   completeness signal: closes the comment hole but keeps the proxy shape
   (source content standing in for runtime behavior) that produced both
   failures. (Reason in Q2(a).)
10. **Reject** — `testthat:::find_test_scripts()` or any frame/internals walk
    to learn run scope: fails open on testthat reorganization with no
    surfacing failure. (Reason in Q2(d).)
11. **Reject** — enforcing per-function against "functions whose owning file
    ran": undefinable for uncovered functions without a hand map. (Reason in
    Q1.)
12. **Reject** — moving the contract out of the suite: advisory-only
    locally, which D-010 forbids. (Reason in Q1.)

## Binding criteria

- **BC1.** `test-zzz-command-contract.R` computes the expected set as exactly
  the files matching `^test-.*\.[Rr]$` in the test directory, reading **no
  file's contents**; a unit test points the (directory-parameterized)
  expected-set function at a fixture directory and asserts the set is
  invariant under appending `# local_fake_tools()` to a member file, and that
  an unparseable `test-garbage.R` fixture is still a member.
- **BC2.** The set of test files that executed is recorded at `test_that`
  execution time by a helper-defined shadow that forwards the unevaluated
  call to `testthat::test_that`; a test running `testthat::test_dir()` on a
  fixture suite asserts that a file whose only test's first statement is
  `skip()` still joins the recorded set.
- **BC3.** The contract file's first test asserts its own file name is in the
  recorded set, and this assertion runs (not skips) under all three
  invocations: full `devtools::test()`, filtered
  `devtools::test(filter = "helper-boundary|zzz")`, and
  `testthat::test_file()` on the contract file alone.
- **BC4.** With file recording broken (`harness_caller_file()` forced to
  `NA_character_`), a full `devtools::test()` reports **at least 1 failure
  and 0 contract skips**; the failure is BC3's assertion.
- **BC5.** On a healthy tree: full `devtools::test()` enforces (0 contract
  skips); `devtools::test(filter = "helper-boundary|zzz")` yields exactly 1
  contract skip and 0 failures, the skip reason naming every non-run test
  file; `testthat::test_file()` on the contract file alone yields 0 failures
  with the completeness comparison skipped and BC3's assertion passed.
- **BC6.** `tests/testthat.R` sets `OPENAC_FULL_SUITE=true` before
  `test_check("openac")`; the skip/fail/enforce decision is a pure function
  of `(expected, ran, covered, domain, deferred, declared_full)` with unit
  tests covering every branch, including: `declared_full = TRUE` with any
  expected file absent from `ran` → **fail** naming the file(s);
  `declared_full = FALSE`, same state → skip naming the file(s); complete run
  with empty `covered` → fail.
- **BC7.** `openac_registry$runs`, `harness_runs()`, `harness_test_files()`,
  and the assembled-needle text search are absent from `tests/testthat/`:
  `grep -rn "harness_runs\|harness_test_files\|local_fake_\" \"tools"
  tests/testthat/` returns no matches.
- **BC8.** The contract file asserts it sorts last among the expected files,
  and mutation evidence in the work log shows: (i) appending a comment
  containing `local_fake_tools(` to a real test file leaves a full run
  enforcing and green; (ii) forcing every test in one real harness file to
  skip makes a full run **fail naming uncovered functions** (not skip).
  Helper/test files restored and `git status` clean afterward.
- **BC9.** `DESCRIPTION` contains no `Config/testthat/parallel: true`; the
  contract file (or a helper test) asserts
  `!isTRUE(as.logical(Sys.getenv("TESTTHAT_PARALLEL")))` or an equivalent
  documented guard, so enabling parallel testing fails the gate loudly
  rather than silently emptying the registry.
- **BC10.** Full `devtools::test()` on this machine (audio.whisper absent):
  **0 failures, pass count ≥ 504** (tolerance: pass count grows with the new
  unit tests, never shrinks), contract enforcing; `devtools::check()`: 0
  errors, 0 warnings (the standing spelling NOTE excepted); `R CMD check`
  green on all five CI platforms.
