# M10: Command-contract coverage gate — completeness observed, not inferred

**Status:** done (2026-08-08, PR #11 https://github.com/jmgirard/openac/pull/11)

**Goal:** Rebuild the command contract's completeness precondition so the gate
enforces on a complete run, names the missing files on a partial one, and cannot
be silently disarmed from inside a test file.

**Outcome:** `helper-openac.R` gains a `test_that` shadow recording each file at
execution time into `openac_registry$ran`, attributed to the EXECUTING file via
testthat's `source_file()` frame; `expected_test_files(dir)` lists names by
testthat's own `^test.*\.[rR]$`; `contract_decision()` is a pure function of six
facts with five named returns; `tests/testthat.R` declares full runs via
`OPENAC_FULL_SUITE`; the contract file gains a first-test canary plus ordering,
`start-first` and parallel assertions. New `test-harness-recording.R` and
`test-contract-decision.R`; test-code only.

**Decisions:** observe-and-declare over parse-tree detection (RR02: parsing keeps
the content-proxy shape that failed twice); attribution credits the executing file.

**Review:** three passes; pass 2 actioned D20 (85, a whole-file `skip_on_cran()`
would fail a declared run — now a candidate) and D8 (80, AC5 required a deletion
that never happened), pass 3 verified the amended criterion; 31 logged below
threshold. Returns: 1 defect, 3 amendment (AC5 twice, second by user override).
Retired M06's sorted-order lesson: the suite now fails on both its mistakes.
