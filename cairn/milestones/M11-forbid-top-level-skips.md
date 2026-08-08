# M11: A wholly-skipped test file cannot exist — the coverage gate's blind spot, closed at the door

- **Status:** review
- **Priority:** normal
- **Depends on:** M10
- **Driving RR:** —
- **Principles touched:** GP7
- **Branch/PR:** `m11-forbid-top-level-skips`

## Goal

Make a test file that skips before any `test_that()` runs impossible to write,
so the coverage gate's completeness observation can never mistake a wholly
skipped file for a file that never ran.

## Scope

**In:** a parse-based scanner over `expected_test_files()` reporting any file
whose top level skips outside a `test_that()` body; an enforced assertion in
`test-zzz-command-contract.R` that the suite is clean; a mutation check proving
the assertion fails when the rule is broken; and a guard that the full-run
declaration in `tests/testthat.R` is actually present, since without it every
incompleteness silently downgrades from a failure to a pass.

**Out:** recording the files testthat *sources* rather than those that execute a
test (the run-observer approach) — rejected at the plan gate, and not deferred;
`local devtools::test()` divergence, which this approach removes rather than
manages; any change to `contract_decision()`'s five returns or to how
completeness itself is observed (D-013 stands unamended).

## Acceptance criteria

- [x] AC1: Over a generated fixture directory holding exactly these twelve
      members, the scanner's result is `identical()` to the sorted vector of the
      seven marked ✓ — reported ✓: `test-top-bare.R` (top-level `skip()`),
      `test-top-qualified.R` (`testthat::skip_on_cran()`), `test-top-indented.R`
      (a `skip()` indented but still top-level), `test-top-guarded.R`
      (`if (TRUE) skip()`), `test-top-local.R` (`local({ skip() })`),
      `test_underscore.R` and `testbare.R` (both top-level `skip()`, both
      discovered by testthat's own pattern); not reported ✗: `test-inner.R`
      (`skip()` inside `test_that()`), `test-clean.R` (no skip),
      `test-skipname.R` (top-level `skipper <- function() NULL`),
      `test-fn-def.R` (top-level `gate <- function() skip()` — defining is not
      skipping), `helper-top-skip.R` (top-level `skip()`, but not a test file).
- [x] AC2: Adding a member that does not parse to AC1's fixture directory
      changes neither the scanner's result nor its success — the same
      `identical()` assertion holds under `expect_no_error()` and
      `expect_no_warning()`.
- [x] AC3: No member of `expected_test_files(test_path("."))` has a top-level
      expression, outside any `test_that()` call and outside any function
      definition, containing a call to a function whose name begins `skip` —
      asserted in `test-zzz-command-contract.R` by the scanner returning
      `character()`, and passing under `devtools::test()`. The claim is exactly
      what that walk enumerates: a top-level call to a locally defined wrapper
      that itself skips is a disclosed residual hole.
- [x] AC4: With a `skip_on_cran()` temporarily hoisted above the first
      `test_that()` of `tests/testthat/test-real-tools.R`, `devtools::test()`
      reports a failure raised by AC3's assertion and naming
      `test-real-tools.R`. Run recorded in the Review section, mutation reverted
      in the same task.
- [x] AC5: A test asserts both that parsing `tests/testthat.R` finds a top-level
      `Sys.setenv()` call setting `OPENAC_FULL_SUITE` to a value
      `declared_full_run()` reads as `TRUE`, and that when
      `Sys.getenv("_R_CHECK_PACKAGE_NAME_")` is non-empty `declared_full_run()`
      is `TRUE`. Deleting that `Sys.setenv()` line makes `devtools::test()`
      fail. Both the passing run and the mutation run recorded in the Review
      section, mutation reverted in the same task.
- [x] AC6: The profile's verify slot is clean — `devtools::document()` produces
      no diff and `devtools::test()` passes. Both outputs recorded.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5, T6
- AC6 → T7

## Tasks

- [x] T1: Add the fixture-directory test to `tests/testthat/test-harness-recording.R`,
      building the twelve members of AC1 via the existing `write_fixture_dir()`
      (test-harness-recording.R:23) and asserting the expected vector. Fails
      until T2.
- [x] T2: Implement the scanner in `tests/testthat/helper-openac.R` beside
      `expected_test_files()` (helper-openac.R:45): parse each member, walk each
      top-level expression whole, excluding the subtrees of `test_that()` calls
      and of `function` definitions, report files containing a call whose name
      begins `skip`; a member that fails to parse is skipped, not reported.
- [x] T3: Add AC3's assertion to `tests/testthat/test-zzz-command-contract.R`
      beside the existing bypassing-forms test (test-zzz-command-contract.R:135),
      with prose stating the rule and its disclosed hole.
- [x] T4: Mutation-verify T3: hoist a `skip_on_cran()` above the first
      `test_that()` of `test-real-tools.R` (test-real-tools.R:51), record the
      failing `devtools::test()` output, revert.
- [x] T5: Add AC5's declaration guard — static parse of `tests/testthat.R` plus
      the `_R_CHECK_PACKAGE_NAME_` conditional (measured 2026-08-08: a vanilla
      `R CMD check` sets it to the package name for the test process).
- [x] T6: Mutation-verify T5: delete the `Sys.setenv()` line at
      tests/testthat.R:22, record the failing `devtools::test()` output, revert.
- [x] T7: Append the DECISIONS entry recording the standing prohibition as an
      extension of D-013's forbidden-forms list; absorb the ROADMAP candidate
      row; run `devtools::document()` and `devtools::test()` clean.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) ran three rounds — 6 findings on the first draft, 6 on the second, 5 on the third, all disposed before commit; decisive ones were a canary pair whose second member had no independent failure mode, fixtures that a column-anchored text grep and a `^test-`-only domain both satisfied, and a declaration guard keyed on an undocumented check internal that failed open.
- 2026-08-08: plan gate chose forbidding top-level skips over a source-time run observer because the observer needed R6 in Suggests, a fix for the runner-before-helper sourcing order, and a widening of D-013's observation rule, while every skip in this suite is already written inside a test; falsified by a real need for a whole-file skip that per-test gating cannot express.
- 2026-08-08 (T1): fixture test added to `test-harness-recording.R`; all three assertions fail with `could not find function "top_level_skips"`, the pre-implementation red T1 plans for.
- 2026-08-08 (T2): `top_level_skips()` and `skip_call_present()` added to `helper-openac.R`; the fixture file is green (19 pass, 0 fail) and the scanner returns `character()` over the real suite.
- 2026-08-08 (T3): assertion added to `test-zzz-command-contract.R`; `devtools::test()` reports 546 pass, 0 fail, 2 skip (OpenFace and whisper absent on this machine).
- 2026-08-08 (T4): mutation verified — a `skip_on_cran()` hoisted above the first `test_that()` of `test-real-tools.R` gives FAIL 1, raised by `test-zzz-command-contract.R:157` with `actual: "test-real-tools.R"`; the completeness check stays green because `NOT_CRAN=true` makes the hoisted call a runtime no-op, so the scanner is the sole cause. Reverted.
- 2026-08-08 (T5): `declaration_present()` added to `helper-openac.R` and asserted in the contract file, with the `_R_CHECK_PACKAGE_NAME_` check as the secondary half; `devtools::test()` reports 548 pass, 0 fail.
- 2026-08-08 (T6): mutation verified — deleting `Sys.setenv(OPENAC_FULL_SUITE = "true")` from `tests/testthat.R` gives FAIL 1 at `test-zzz-command-contract.R:151`, `Expected declaration_present(runner) to be TRUE`. Reverted; suite back to 548 pass.
- 2026-08-08 (T7): D-015 appended; the ROADMAP candidate row was absorbed into the M11 row at plan time, so nothing was left to prune. `devtools::document()` produced no diff and `devtools::test()` is clean at 548 pass, 0 fail, 2 skip.
- 2026-08-08 (review): four findings scored ≥80 fixed on the branch — an applied-function-expression hole, an order-blind declaration check, `do.call`-held skip callees, and an over-broad `^skip` prefix; regression tests added in their own fixture directory and D-015 corrected to three disclosed holes. Suite 557 pass, `check()` 0/0/1 (the standing spelling NOTE).
- 2026-08-08: plan chose a parse walk excluding `test_that()` and `function` subtrees over a top-level call-head match, because the head match misses `if (cond) skip()`, `local({ skip() })` and `suppressWarnings(skip_on_cran())`, each of which aborts a file identically; falsified by a skip form that aborts a file while sitting inside one of the two excluded subtrees.

## Decisions

## Review

**PR:** https://github.com/jmgirard/openac/pull/12

### Acceptance-criterion evidence (2026-08-08, all re-executed at review)

- AC1: the twelve-member fixture directory rebuilt and `top_level_skips()` called
  directly — reported the seven expected members, and `identical()` against the
  sorted expectation returned TRUE. Also green as a suite assertion
  (`test-harness-recording.R`, 19 pass).
- AC2: a non-parsing member added to that same directory — result `identical()`
  to the seven-member expectation, no error, and no warning surfaced by a
  `withCallingHandlers()` probe.
- AC3: `top_level_skips("tests/testthat")` reported 0 files; the assertion at
  `test-zzz-command-contract.R:180` passes in a full `devtools::test()`
  (557 pass, 0 fail, 2 skip — OpenFace and whisper absent on this machine).
  Re-run after the review fixes below; the count rose from 548 with their
  regression tests.
- AC4: mutation re-run at review — `skip_on_cran()` hoisted above the first
  `test_that()` of `test-real-tools.R` gives FAIL 1, raised by
  `test-zzz-command-contract.R:180` with `actual: "test-real-tools.R"`; the
  criterion's "raised by AC3's assertion and naming that file" is met by that
  line and value. Reverted, tree clean.
- AC5: static half green in the same 548-pass run; mutation re-run at review —
  deleting `Sys.setenv(OPENAC_FULL_SUITE = "true")` from `tests/testthat.R`
  gives FAIL 1 at `test-zzz-command-contract.R:151`,
  `Expected declaration_present(runner) to be TRUE`. Reverted, tree clean.
- AC6: `devtools::document()` produced no diff (`git status` empty after the
  run); `devtools::test()` passes at 557.

### Independent review (three lenses + scorer)

Prior-review lens: no candidate regressions; the inline-comment probe returned
empty, so that surface was skipped. Blame/history lens: no reintroduction of the
retired proxy shapes, no lesson falsified, D-015's claims about D-010/D-013/D-014
verified. It also reported one non-reproducible failure of the new declaration
guard; run down and explained — its signature is byte-identical to the AC5
mutation deliberately applied to this shared tree minutes earlier, and three
subsequent clean runs plus a direct call returning TRUE confirm it. Not a flake
in the diff; a collision caused by mutating a shared checkout while subagents
were live.

29 findings scored; 4 scored ≥80 and were **all fixed on the branch**:

- F1 (88) — an immediately-invoked `(function() skip())()` at top level aborts
  the file and the guard read FALSE. Fixed: the `function` exclusion is now
  application-aware, and the `(` wrapper the AST keeps around an IIFE is
  unwrapped. That wrapper is why the first fix attempt still missed both IIFE
  forms — measured, not reasoned.
- F11 (87) — `declaration_present()` used `any()` over top-level expressions, so
  a `Sys.setenv()` placed *after* `test_check()`, a later `Sys.unsetenv()`, and a
  later re-set to `"false"` all read as declared. Fixed: it now walks in order
  and keeps the state the run actually starts with. This also fixes F12 (78) and
  F14 (65), where a non-literal value raised a coercion error instead of
  answering FALSE.
- F3 (84) — `do.call(skip, ...)` and `do.call("skip", ...)` hold the callee as a
  symbol or string, so no call to `skip` existed for the walk to find. Fixed for
  the named-callee forms; a computed callee stays disclosed.
- F4 (80) — the `^skip` prefix reported any call merely *named* like `skipper()`.
  Fixed: `^skip($|_)`, which still covers testthat's whole skip surface.

Regression tests for all four live in their own fixture directory, so AC1's
twelve-member assertion stays exactly as written. D-015 and the contract-file
prose were corrected to state three disclosed holes rather than one, since the
fixes changed which forms are caught.

25 findings scored below 80 — logged, not actioned. In brief: F2 (58) a skip
inside a lambda handed to an applier, F17 (55) a masked assertion binding, F19
(52) mutation checks that do not probe the surviving modes, F5 (48) quoted
skips reported, F20 (50) a call-name idiom now duplicated four times, F7/F18
(50/45) parse-warning and unreadable-file gaps, F13 (35) `test_check()` presence
unasserted, F10 (35) the helper/setup domain undocumented in D-015 (since
corrected anyway), F21 (30), F8 (25) a parse-encoding divergence the reviewer
could not make fail, F9 (15) a redundant `unname()`, F6 (65) the scanner being
stricter than its "before its tests run" name, F15 (63) the two new scanners
differing on subtree walking, F22 (68) prose overstating exhaustiveness (fixed
anyway as a consequence of F1/F3), F23 (5) stale, and B1–B7 (5–12) clean-bill or
explained.

Return floor: none of the four actioned findings falsifies an acceptance
criterion inside the domain its wording names — F1 and F2 sit inside a
function-definition subtree, which AC3 explicitly excludes — and none scored ≥90
on user-facing deliverable behavior. So no status return; all four were
fix-now.

### Consistency gate

- CI on PR #12: pass on all five jobs — ubuntu release / devel / oldrel-1,
  macos-latest, windows-latest — re-run after the review fixes (run
  31277448344). These are the runs in which `R CMD check` declares a full
  suite, so the contract gate enforced rather than skipped.
- `cairn_validate.py`: all checks passed, exit 0.
- `cairn_impact.py`: not run — no DESIGN.md principle changed.
- `devtools::check()` (re-run after the review fixes): 0 errors, 0 warnings, 1 NOTE — the standing spelling
  NOTE (its own ROADMAP candidate). Every hit it lists is in an `.Rd`,
  `NEWS.md`, `README.md` or a vignette; `git diff --name-only main..HEAD` shows
  this branch touches no such path, so the NOTE is unchanged from the default
  branch by construction rather than by comparison of remembered counts (the
  M06 lesson's trap).
- Profile `consistency-gate`: `document()` no-diff ✓; generated files not
  hand-edited (the diff touches no `R/`, `man/`, `NAMESPACE` or `DESCRIPTION`
  path) ✓; README.Rmd untouched, so no re-knit owed ✓; no `_pkgdown.yml` in the
  repo, so the pkgdown check no-ops ✓; no NEWS entry owed — the milestone is
  test-code and tracking only, with no user-visible change ✓; no new top-level
  files ✓.
