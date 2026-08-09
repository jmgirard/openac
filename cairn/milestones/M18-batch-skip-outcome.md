# M18: A skipped file is a skip, not a success

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M17
- **Driving RR:** —
- **Principles touched:** GP6
- **Branch/PR:** `m18-batch-skip-outcome`

## Goal

Give the batch outcome table a third state, so a file the batch deliberately
did not process is recorded as skipped rather than as a success.

## Scope

**In:** `dir_walk()` classifies a row only by whether `.f` raised an error
(`R/utils.R:118-129`), so a single-file function that deliberately declines a
file returns normally and the row reads `success = TRUE, error = NA`. A skip
condition class carries "I did not process this file, and here is why";
`dir_walk()` catches it and records a third state. The outcome table gains a
`status` column (`"ok"` / `"skipped"` / `"failed"`); `success` becomes
`status == "ok"`, so a skipped row reads `success = FALSE`, and `error`
carries the reason for a skipped row as it already does for a failed one.
Three deliberate-skip sites signal it: `aw_transcribe()`'s no-audio skip
(`R/use_whisper.R:296-300`) and the `overwrite = FALSE` skips in
`aw_prep_audio()` (`R/use_whisper.R:111-113`) and `os_prep_audio()`
(`R/use_opensmile.R:184-186`). Five `@return` blocks, NEWS, the KNOWN GAP test.

**Out:** Non-zero tool exits → M17 (a failed run is a failure, not a skip).
Guards whose message names no file → M19. Output-path collisions → the
standing ROADMAP candidate; the skip channel this milestone adds is the
plumbing that work needs, so it is planned after this one lands, not now.

## Acceptance criteria

- [ ] AC1 `dir_walk()` records three states: a test drives a `.f` that
      returns normally, one that signals the skip condition, and one that
      errors, and asserts the rows read `status` of `"ok"`, `"skipped"` and
      `"failed"` respectively, with `success` `TRUE`, `FALSE`, `FALSE`.
- [ ] AC2 Every `*_dir()` wrapper returns a table carrying a `status` column,
      as does the zero-row table `dir_walk()` returns for an empty input
      (`R/utils.R:112-114`), and each wrapper's roxygen `@return` documents
      the three values. The test derives its wrapper list at run time from the
      `_dir$` names in `asNamespace("openac")`, not from the `R/*.R` sources
      (an installed package's `R/` holds only the lazy-load database, so a
      source grep matches nothing under `R CMD check` and the test would go
      vacuous exactly where it gates the merge), so a sixth wrapper reds the
      test until it is covered — the computed-domain shape D-010 adopted for
      the command contract.
- [ ] AC3 The three deliberate-skip sites named in Scope signal the skip
      condition instead of returning normally, and `aw_transcribe()`'s
      combined branch (`R/use_whisper.R:296-300`) is split so the two facts it
      currently conflates part company: a file with no audio stream is
      `"skipped"`, a file `ffp_count_streams()` could not probe is `"failed"`.
      One test per site asserts `status == "skipped"`, `success == FALSE` and
      the reason in `error`; a fourth drives an unprobeable file through
      `aw_transcribe_dir()` and asserts `status == "failed"`.
- [ ] AC4 The KNOWN GAP test in `tests/testthat/test-batch-dirs.R` — whatever
      M17 leaves of it, having rewritten its `os_prep_audio_dir` half — is
      replaced by tests asserting the skip state, and
      `dir_walk_reports_failure()` (`tests/testthat/helper-openac.R:882`) is
      updated or retired together with the comment that anticipates this fix
      (`:872-881`); a test pins that adding `status` has not made its
      `setdiff(names(x), known)` clause true for every table.
- [ ] AC5 NEWS records the return-shape change and what a caller reading
      `success` must do differently; `devtools::document()` shows no drift,
      `devtools::test()` passes, and `devtools::check()` reports 0 errors, 0
      warnings and no NOTE other than the pre-existing `spelling` NOTE.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T4
- AC3 → T1, T3
- AC4 → T5
- AC5 → T4, T6

## Tasks

- [ ] T1 Test-first: the three-state `dir_walk()` tests and the three
      per-site skip tests, red before the change.
- [ ] T2 Add the skip condition constructor and the third state in
      `dir_walk()` (`R/utils.R:111-140`), including the zero-row branch
      (`R/utils.R:112-114`) whose column set must match.
- [ ] T3 Convert the three deliberate-skip sites to signal the condition.
- [ ] T4 Update the five `@return` blocks (`R/use_whisper.R:199-202`, `:428-431`;
      `R/use_opensmile.R:229-232`, `:403-406`; `R/use_openface.R:126-129`) and
      run `devtools::document()`.
- [ ] T5 Replace the KNOWN GAP test and update the helper and its comment.
- [ ] T6 NEWS entry; `devtools::test()`; `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned three findings on this file — AC3 tickable in a state recording an unprobeable file as "skipped" (the `is.na(has_audio) || !has_audio` branch conflates two facts), AC2 blind to `dir_walk()`'s zero-row column set and hand-listing what its own grep enumerates, AC4 anchored on a line M17 rewrites first; all three fixed before the gate, none deferred.
- 2026-08-08: plan gate chose a third outcome state over treating a deliberate skip as a failure because re-running a completed batch would then report every already-done file as failed; falsified by a user reading `success` for whom skip and failure are the same disposition.
- 2026-08-08: plan gate chose a skip *condition class* over a sentinel return value because `dir_walk()` inspects no return value and the `do.call` paths (`aw_transcribe_dir`, `os_extract_dir`) return heterogeneous values a sentinel would have to be distinguished from; falsified by a skip needing to carry structured data a condition cannot.

- 2026-08-09: implement gate chose a non-error skip condition (a direct call still returns as it always did), an abort for a file ffprobe cannot read, and an informational rather than warning line per skipped file.
- 2026-08-09: T1 `tests/testthat/test-batch-skip-outcome.R` written and RUN red before any source change — 10 failures, each on the absent `status` column or on `skip_file` not existing; box unticked until the suite is green.
- 2026-08-09: amendment — AC2's run-time wrapper list moves from a `R/*.R` grep to `asNamespace("openac")`; MEASURED an installed package's `R/` holds only the lazy-load DB (withr: `withr`, `withr.rdb`, `withr.rdx`), so the grep would match nothing under `R CMD check` and the criterion would be vacuous there.

## Decisions

## Review
