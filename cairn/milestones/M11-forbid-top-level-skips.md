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

- [ ] AC1: Over a generated fixture directory holding exactly these twelve
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
- [ ] AC2: Adding a member that does not parse to AC1's fixture directory
      changes neither the scanner's result nor its success — the same
      `identical()` assertion holds under `expect_no_error()` and
      `expect_no_warning()`.
- [ ] AC3: No member of `expected_test_files(test_path("."))` has a top-level
      expression, outside any `test_that()` call and outside any function
      definition, containing a call to a function whose name begins `skip` —
      asserted in `test-zzz-command-contract.R` by the scanner returning
      `character()`, and passing under `devtools::test()`. The claim is exactly
      what that walk enumerates: a top-level call to a locally defined wrapper
      that itself skips is a disclosed residual hole.
- [ ] AC4: With a `skip_on_cran()` temporarily hoisted above the first
      `test_that()` of `tests/testthat/test-real-tools.R`, `devtools::test()`
      reports a failure raised by AC3's assertion and naming
      `test-real-tools.R`. Run recorded in the Review section, mutation reverted
      in the same task.
- [ ] AC5: A test asserts both that parsing `tests/testthat.R` finds a top-level
      `Sys.setenv()` call setting `OPENAC_FULL_SUITE` to a value
      `declared_full_run()` reads as `TRUE`, and that when
      `Sys.getenv("_R_CHECK_PACKAGE_NAME_")` is non-empty `declared_full_run()`
      is `TRUE`. Deleting that `Sys.setenv()` line makes `devtools::test()`
      fail. Both the passing run and the mutation run recorded in the Review
      section, mutation reverted in the same task.
- [ ] AC6: The profile's verify slot is clean — `devtools::document()` produces
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
- 2026-08-08: plan chose a parse walk excluding `test_that()` and `function` subtrees over a top-level call-head match, because the head match misses `if (cond) skip()`, `local({ skip() })` and `suppressWarnings(skip_on_cran())`, each of which aborts a file identically; falsified by a skip form that aborts a file while sitting inside one of the two excluded subtrees.

## Decisions

## Review
