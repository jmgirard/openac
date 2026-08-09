# M18: A skipped file is a skip, not a success

- **Status:** review
- **Priority:** normal
- **Depends on:** M17
- **Driving RR:** —
- **Principles touched:** GP6
- **Branch/PR:** `m18-batch-skip-outcome` — https://github.com/jmgirard/openac/pull/19

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

- [x] AC1 `dir_walk()` records three states: a test drives a `.f` that
      returns normally, one that signals the skip condition, and one that
      errors, and asserts the rows read `status` of `"ok"`, `"skipped"` and
      `"failed"` respectively, with `success` `TRUE`, `FALSE`, `FALSE`.
- [x] AC2 Every `*_dir()` wrapper returns a table carrying a `status` column,
      as does the zero-row table `dir_walk()` returns for an empty input
      (`R/utils.R:112-114`), and each wrapper's roxygen `@return` documents
      the three values. The test derives its wrapper list at run time from the
      `_dir$` names in `asNamespace("openac")`, not from the `R/*.R` sources
      (an installed package's `R/` holds only the lazy-load database, so a
      source grep matches nothing under `R CMD check` and the test would go
      vacuous exactly where it gates the merge), so a sixth wrapper reds the
      test until it is covered — the computed-domain shape D-010 adopted for
      the command contract.
- [x] AC3 The three deliberate-skip sites named in Scope signal the skip
      condition instead of returning normally, and `aw_transcribe()`'s
      combined branch (`R/use_whisper.R:296-300`) is split so the two facts it
      currently conflates part company: a file with no audio stream is
      `"skipped"`, a file `ffp_count_streams()` could not probe is `"failed"`.
      One test per site asserts `status == "skipped"`, `success == FALSE` and
      the reason in `error`; a fourth drives an unprobeable file through
      `aw_transcribe_dir()` and asserts `status == "failed"`.
- [x] AC4 The KNOWN GAP test in `tests/testthat/test-batch-dirs.R` — whatever
      M17 leaves of it, having rewritten its `os_prep_audio_dir` half — is
      replaced by tests asserting the skip state, and
      `dir_walk_reports_failure()` (`tests/testthat/helper-openac.R:882`) is
      updated or retired together with the comment that anticipates this fix
      (`:872-881`); a test pins that adding `status` has not made its
      `setdiff(names(x), known)` clause true for every table.
- [x] AC5 NEWS records the return-shape change and what a caller reading
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

- [x] T1 Test-first: the three-state `dir_walk()` tests and the three
      per-site skip tests, red before the change.
- [x] T2 Add the skip condition constructor and the third state in
      `dir_walk()` (`R/utils.R:111-140`), including the zero-row branch
      (`R/utils.R:112-114`) whose column set must match.
- [x] T3 Convert the three deliberate-skip sites to signal the condition.
- [x] T4 Update the five `@return` blocks (`R/use_whisper.R:199-202`, `:428-431`;
      `R/use_opensmile.R:229-232`, `:403-406`; `R/use_openface.R:126-129`) and
      run `devtools::document()`.
- [x] T5 Replace the KNOWN GAP test and update the helper and its comment.
- [x] T6 NEWS entry; `devtools::test()`; `devtools::check()`.
- [x] T7 Review round 1, F1/F2: regression tests, red before the fix — a
      batch reusing an already-prepared wav under `overwrite = FALSE` still
      runs its tool and records `"ok"`, for `os_extract_dir()` and
      `aw_transcribe_dir()` alike.
- [x] T8 Review round 1, F1/F2: a skip signalled by a NESTED prep call stops
      at that call, so only a batch whose own job was the prep records
      `"skipped"`.
- [x] T9 Review round 1, F3: the leaked `#'` out of the five `@return`
      blocks; `devtools::document()`.
- [x] T10 Re-verify: `devtools::document()` no drift, `devtools::test()`,
      `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned three findings on this file — AC3 tickable in a state recording an unprobeable file as "skipped" (the `is.na(has_audio) || !has_audio` branch conflates two facts), AC2 blind to `dir_walk()`'s zero-row column set and hand-listing what its own grep enumerates, AC4 anchored on a line M17 rewrites first; all three fixed before the gate, none deferred.
- 2026-08-08: plan gate chose a third outcome state over treating a deliberate skip as a failure because re-running a completed batch would then report every already-done file as failed; falsified by a user reading `success` for whom skip and failure are the same disposition.
- 2026-08-08: plan gate chose a skip *condition class* over a sentinel return value because `dir_walk()` inspects no return value and the `do.call` paths (`aw_transcribe_dir`, `os_extract_dir`) return heterogeneous values a sentinel would have to be distinguished from; falsified by a skip needing to carry structured data a condition cannot.

- 2026-08-09: implement gate chose a non-error skip condition (a direct call still returns as it always did), an abort for a file ffprobe cannot read, and an informational rather than warning line per skipped file.
- 2026-08-09: T1 `tests/testthat/test-batch-skip-outcome.R` written and RUN red before any source change — 10 failures, each on the absent `status` column or on `skip_file` not existing; box unticked until the suite is green.
- 2026-08-09: T2 `skip_file()` (a non-error condition) + `dir_walk()`'s third state; `status` added to the populated and zero-row tables alike.
- 2026-08-09: T3 the three deliberate-skip sites signal it; `aw_transcribe()`'s combined branch split — an unprobeable file now aborts, a probed file with no audio skips. Its direct-call message survives by sitting AFTER the signal, which `dir_walk()`'s exiting handler unwinds past.
- 2026-08-09: T3 discovered sub-task — `test-whisper-transcribe.R:278` pinned the old conflated branch; rewritten to assert the abort, keeping its warning-based discriminator for the NA path.
- 2026-08-09: T5 KNOWN GAP test replaced by one asserting both split states; `dir_walk_reports_failure()` RETIRED (its sole caller was that test, and its column-set proxy would now report every table) with a comment in its place, and its guarantee re-pinned directly by an all-ok batch test asserting the exact column set.
- 2026-08-09: T4 five `*_dir()` `@return` blocks document the `status` vocabulary and that `success` is `status == "ok"`; both `overwrite` `@param`s note the batch now records the skip; `devtools::document()` re-run, `devtools::test()` clean (803 pass).
- 2026-08-09: T6 NEWS entry added and the previous entry's now-false tail ("`aw_transcribe_dir()` … still records such a file as a success") corrected in place; `devtools::check()` **Status: OK** — 0 errors, 0 warnings, 0 notes, the spelling comparison clean after rewording two words the change introduced.
- 2026-08-09: DESIGN Known-issues corrected in place (marked, 2026-08-09 M18) on two claims this milestone falsified — `aw_transcribe` skipping an unprobeable file, and `aw_transcribe_dir()` recording it as a success — and the three-state table recorded there.
- 2026-08-09: amendment — AC2's run-time wrapper list moves from a `R/*.R` grep to `asNamespace("openac")`; MEASURED an installed package's `R/` holds only the lazy-load DB (withr: `withr`, `withr.rdb`, `withr.rdx`), so the grep would match nothing under `R CMD check` and the criterion would be vacuous there.
- 2026-08-09: review round 1 checkpoint — PR #19 opened as a draft; consistency gate green (`cairn_validate` exit 0, `check()` Status: OK, `document()` no drift, 803 tests pass); all five criteria ticked against recorded evidence. Two defects found while gathering it (a leaked `#'` in the five rendered `@return` blocks, and the skip signal unwinding past the work `overwrite = FALSE` was meant to preserve in `os_extract_dir`/`aw_transcribe_dir`); fresh-context review still in flight, triage pending.
- 2026-08-09: review round 1 RETURNED to in-progress under the return floor. F1 (scored 90) — `dir_walk()`'s `tryCatch` handler for `openac_file_skipped` is EXITING, so an `overwrite = FALSE` skip signalled by a NESTED `os_prep_audio()`/`aw_prep_audio()` unwinds the whole `.f` call: MEASURED, `os_extract_dir(wavdir=, aggdir=, overwrite = FALSE)` over a file whose wav already exists never calls openSMILE and writes no CSV, and F2 (88) is the same on the whisper path. F3 (95) — a literal `#'` leaks into the rendered prose of all five `@return` blocks and into all five `man/*_dir.Rd`. 11 further findings logged below the action bar. Criteria unticked with the return; gate checks were green and are recorded in the Review section.
- 2026-08-09: implement round 2 gate chose that a batch reusing an already-prepared wav records `"ok"`, not `"skipped"` — `status` describes the batch's OWN job, so only a batch whose job was the prep skips; falsified by a caller who needs the table to surface that an internal stage was reused.
- 2026-08-09: T7 three regression tests appended to `test-batch-skip-outcome.R` and RUN red before any source change — 8 failures: `os_extract_dir()` and `aw_transcribe_dir()` over a file whose wav already exists read `status = "skipped"` with the tool never reached and no output file written. The third test passes already and is the boundary the fix must not move (`os_prep_audio_dir()` still skips). Box unticked until the suite is green.
- 2026-08-09: T8 `absorb_skip()` added beside `skip_file()`; the two NESTED prep calls (`os_extract()` at `R/use_opensmile.R:318`, `aw_transcribe()`'s `do.call` at `R/use_whisper.R:340`) wrap theirs in it, so a reused-wav skip stops at the prep call instead of unwinding the per-file job. The two `*_prep_audio_dir()` wrappers call the prep function as `.f` directly and are untouched, which is why their skip still reaches `dir_walk()`. T7's 8 failures now pass; full suite 813 pass, 0 fail.
- 2026-08-09: T9 the leaked `#'` removed from all five `@return` blocks (5 replacements, asserted); `devtools::document()` re-run and `grep -rn "its #'" R/ man/` now matches nothing. Two round-1 findings logged below the action bar fixed in the same lines rather than left in text being rewritten anyway: the retained "a file that fails is skipped with a warning" sentence, which contradicted the `"skipped"`/`"failed"` vocabulary defined two sentences above it, and `R/use_whisper.R:310`'s claim that `os_prep_audio()` aborts an unprobeable input — it never counts streams and has no such branch.
- 2026-08-09: T10 re-verified after the round-1 fixes — `devtools::document()` no drift, `devtools::test()` 813 pass / 0 fail (10 more than round 1's 803, the three new regression tests), `devtools::check()` **Status: OK** 0/0/0 with the spelling comparison clean. NEWS narrowed in the same pass: it named the three skip sites without saying that `status` describes the batch's OWN job, so a reader would have expected `os_extract_dir(wavdir=, overwrite = FALSE)` to report a skip where it now reports `"ok"`. Status back to review.
- 2026-08-09: review round 2 — gate re-run clean (`cairn_validate` exit 0, `check()` Status: OK 0/0/0, `document()` no drift, 813 tests pass) and all five criteria re-ticked against fresh round-2 evidence; CI green on all five platforms. Fresh-context review in flight.

## Decisions

## Review

_Round 1 — 2026-08-09. Fresh evidence gathered on `m18-batch-skip-outcome`
at 8144b17, level with `origin/main`. The five criterion
ticks recorded below were UNTICKED when the round closed: F1/F2/F3 change the
artifact each was measured against, so this evidence is a round-1 record, not a
standing verification._

### Acceptance-criterion evidence

- **AC1** — `devtools::test(filter = "batch-skip-outcome")` green, 40
  assertions. `test-batch-skip-outcome.R:16-40` drives one `.f` over three
  inputs (returns normally / `skip_file()` / `stop()`) and asserts
  `status` `c("ok", "skipped", "failed")` with `success`
  `c(TRUE, FALSE, FALSE)`; `error` is `NA` for the ok row and carries the
  reason for the other two.
- **AC2** — same run. `:120-156` computes the wrapper domain from
  `grep("_dir$", ls(asNamespace("openac")))` and `expect_setequal()`s it
  against the five covered names, then asserts `status` present in each
  wrapper's returned table; `:82-98` pins the zero-row table's column set
  identical to a populated one (`infile`, `status`, `success`, `error`).
  All five `@return` blocks document the three values (`git diff` on
  `R/use_whisper.R`, `R/use_opensmile.R`, `R/use_openface.R`) — but see
  finding **F1**: a stray `#'` leaked into the rendered prose.
- **AC3** — same run. `:160-238` gives one test per site:
  `aw_prep_audio_dir()` and `os_prep_audio_dir()` under `overwrite = FALSE`
  read `status == "skipped"`, `success == FALSE`, `error` matching
  `"overwrite"`, with `boundary_tools()` empty (nothing reached a tool); the
  no-audio file reads `"skipped"` after exactly one `ffprobe`; the
  unprobeable file reads `"failed"` with `"could not be counted"`. The split
  is real in source (`R/use_whisper.R:302-330`).
- **AC4** — `devtools::test()` full suite green, 803 pass / 0 fail. The
  KNOWN GAP test is gone; `test-batch-dirs.R:357-385` replaces it and
  asserts both split states (`c("skipped", "failed")`, both
  `success = FALSE`). `dir_walk_reports_failure()` is retired with a comment
  in its place (`helper-openac.R:872-881`), and its `setdiff(names(x), known)`
  guarantee is re-pinned directly by the all-ok batch test
  (`test-batch-skip-outcome.R:100-118`), which asserts the exact column set
  on a table where nothing failed.
- **AC5** — `NEWS.md` gains the return-shape entry naming what a `success`
  reader must do differently, and the previous entry's now-false tail is
  corrected in place. `devtools::document()` re-run: no drift (`git status`
  clean but for this tracking file). `devtools::check()` **Status: OK** —
  0 errors, 0 warnings, 0 notes (the criterion allowed a `spelling` NOTE;
  none appeared).

### Consistency gate

- `cairn_validate.py` exit 0 — 16 CHECKs PASS, 8 advisories OK.
- No `DESIGN.md` IP/GP principle changed (the diff sits in *Known issues*,
  `:253-275`), so `cairn_impact --changed` is a clean no-op.
- `r-package` `consistency-gate` slot: `document()` no diff · generated files
  regenerate clean · `README.Rmd`/`README.md` untouched by this branch, so
  their sync state is unchanged from the default branch · no `_pkgdown.yml`
  in the repo, so no pkgdown check · `NEWS.md` (the declared changelog) has
  this milestone's entry, with no milestone numbers in it · no new top-level
  files · `check()` clean.

### Fresh-context review — round 1

Three fresh-context reviewers, distinct evidence bases, then a scorer that
generated none of the findings. The blame-history lens reported one finding
(the leaked `#'`, the same defect as F3 below). The prior-review lens found
no regression: it read the `## Review` sections of the M14 and M17 archives,
which are the milestones that touched these files, and probed
`repos/jmgirard/openac/pulls/comments` — `[]`, so no GitHub thread surface
exists to walk. 14 candidate findings scored; three at or above 80.

**Actioned — all three return the milestone to `in-progress`.**

- **F1 (90) — an `overwrite = FALSE` skip aborts the whole pipeline step
  under a batch.** `dir_walk()` catches `openac_file_skipped` with
  `tryCatch`, an *exiting* handler, so it unwinds `.f` entirely rather than
  just the nested `os_prep_audio()` call that signalled. MEASURED here and
  by the reviewer independently: `os_extract_dir(indir, "mp4", wavdir = w,
  aggdir = a, overwrite = FALSE)` with `w/a.wav` already present reaches the
  boundary twice (`ffprobe`, `ffprobe`), never calls openSMILE, writes no
  CSV, and records `status = "skipped"`. The same call before this branch —
  and the direct `os_extract()` call today, where the signal is inert
  because nothing handles it — reaches `opensmile` and writes the CSV. The
  "resume an interrupted batch by reusing the wavs it already prepared"
  idiom silently stopped producing features, and the row reporting a skip is
  reporting a skip of work the caller did want done. Fix direction offered:
  a calling handler plus `rlang::cnd_muffle()` on the restart
  `rlang::signal()` already establishes, or signalling only from a top-level
  `.f`, so a nested decline cannot cancel the rest of the step.
- **F2 (88) — the same defect on the whisper path.** MEASURED:
  `aw_transcribe_dir(indir, "mp4", wavdir = w, audio_args =
  list(overwrite = FALSE))` with `w/a.wav` present records
  `status = "skipped"` with zero whisper calls and no `.rds`/`.csv`; before
  this branch `aw_prep_audio()` returned `"Skipped"` and `aw_transcribe()`
  went on to transcribe the existing wav.
- **F3 (95) — a literal `#'` leaks into the rendered docs of all five
  `@return` blocks.** `R/use_openface.R:128`, `R/use_opensmile.R:238`,
  `:426`, `R/use_whisper.R:208`, `:459` each read `it was called with, its
  #'   \`status\`, whether it \`success\`ed`, and `document()` propagated it
  verbatim into all five `man/*_dir.Rd`. `R CMD check` passes it as valid
  Rd, which is why AC5's no-drift check read clean over it.

**Logged below the action bar — 11 findings, surfaced not dropped.**

- (70) The retained sentence "A file that fails is skipped with a warning"
  in the same five `@return` blocks now contradicts the `"skipped"` /
  `"failed"` vocabulary defined two sentences above it.
- (65) `R/use_whisper.R:308-309`'s new comment says `os_prep_audio()`
  already aborts on an unprobeable input; it never calls
  `ffp_count_streams()` and has no NA branch.
- (55) The AC2 wrapper test runs all five wrappers over an *empty* directory,
  so it exercises only `dir_walk()`'s zero-row branch; its `info =` label is
  positional against a separate vector; its comment says "a seventh".
- (50) No populated `*_dir()` table (carrying `wavfile`/`aggfile`/…) has its
  column set pinned anywhere — only `dir_walk()`'s own one-column input.
- (35) `success = FALSE` for a skip makes a `while (any(!res$success))`
  re-run loop non-terminating under `overwrite = FALSE`; NEWS names the
  `success` change but not this consequence.
- (28) The "announced but does not warn" test depends on the handler being
  exiting to mean what it claims.
- (28) `aw_transcribe()`'s direct path signals *and* warns, so a caller
  installing a calling handler would get both reports.
- (28) A missing ffprobe now yields N failed rows rather than one
  installation error on the `aw_transcribe_dir()` path.
- (25) One `cli_alert_info()` per skipped file still prints N lines on a
  500-file re-run.
- (20) The zero-row branch omits `stringsAsFactors = FALSE` — pre-existing,
  the diff followed the existing pattern.
- (10) `skip_file()` carries prose and no structured field; the plan's
  Scope-Out defers the first consumer that would want one.

**Disposition: return floor (M130) — F1 at 90 is a defect in what this
package does for its users, so the milestone goes back to `in-progress`.**
Defect returns for M18: 1.

---

_Round 2 — 2026-08-09, at cb757ae, `origin/main` unmoved since the branch was
cut. Fresh evidence, re-executed; nothing carried over from round 1._

### Acceptance-criterion evidence

- **AC1** — `test_local(filter = "batch-skip-outcome")` green, 50 assertions
  (40 in round 1, plus the 10 the F1/F2 regression tests add).
  `test-batch-skip-outcome.R:16-40` drives one `.f` over three inputs and
  asserts `status` `c("ok", "skipped", "failed")` with `success`
  `c(TRUE, FALSE, FALSE)`, `error` `NA` for the ok row and the reason for the
  other two.
- **AC2** — same run. `:120-156` computes the wrapper domain from
  `ls(asNamespace("openac"))` and asserts `status` in each of the five
  wrappers' tables; `:82-98` pins the zero-row table's columns identical to a
  populated one. The five `@return` blocks now render clean — round 1's
  leaked `#'` is gone (`grep -rn "its #'" R/ man/` matches nothing) and each
  of the five `man/*_dir.Rd` `\value{}` sections names `status` three times
  (the vocabulary, the `success` identity, the `error` note).
- **AC3** — same run, `:160-238`: the two `overwrite = FALSE` sites and the
  no-audio site each read `"skipped"`/`success = FALSE` with the reason in
  `error` and nothing reaching the tool boundary beyond the deciding probe;
  the unprobeable file reads `"failed"` with `"could not be counted"`.
- **AC4** — `test_local(filter = "batch-dirs")` green, 62 assertions. The
  KNOWN GAP test is replaced (`test-batch-dirs.R:357-385`, both split states,
  both `success = FALSE`); `dir_walk_reports_failure()` is retired with a
  comment in its place, and its column-set guarantee is re-pinned by the
  all-ok batch test (`test-batch-skip-outcome.R:100-118`).
- **AC5** — `NEWS.md` carries the return-shape entry, narrowed in round 2 so
  it no longer implies a reused-wav batch reports a skip. `document()`
  re-run: no drift (`git status` clean). `devtools::test()` 813 pass / 0 fail.
  `devtools::check()` **Status: OK** — 0 errors, 0 warnings, 0 notes.

### Consistency gate

- `cairn_validate.py` exit 0 — 16 CHECKs PASS, 8 advisories OK.
- No `DESIGN.md` IP/GP principle changed, so `cairn_impact --changed` no-ops.
- `r-package` `consistency-gate` slot: `document()` no diff · generated files
  regenerate clean · `README.Rmd`/`README.md` untouched by this branch · no
  `_pkgdown.yml` · `NEWS.md` has this milestone's entry, no milestone numbers
  in it · no new top-level files · `check()` clean.

### Round-1 fixes, verified

The three actioned findings are closed and each has a test that fails
without its fix. F1/F2: `absorb_skip()` (`R/utils.R:122-142`) stops a skip
raised by a nested prep call, and the two nested sites wrap theirs; the
regression tests were run red first (8 failures, both batches recording
`"skipped"` with the tool never reached and no output written) and now
assert the tool ran and the output landed, not `status` alone. The boundary
the fix must not move has its own test: `os_prep_audio_dir()`, where the prep
IS the job, still skips. F3: the leaked `#'` is out of all five blocks.
