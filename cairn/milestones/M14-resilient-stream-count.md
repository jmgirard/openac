# M14: A bad file is an outcome, not the end of the batch

- **Status:** review
- **Priority:** normal
- **Depends on:** M13
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** `m14-resilient-stream-count` · https://github.com/jmgirard/openac/pull/17

## Goal

Make `ffp_count_streams()` report an unprobeable file rather than abort on it, so
a batch records that file as a failed row instead of dying on it.

## Scope

**In:** `ffp_count_streams()` returning `NA` counts with a warning instead of
`stopifnot(file.exists())` plus an ffprobe abort (`R/use_ffprobe.R:46-67`); its
four call sites given an explicit `NA` disposition; a batch test proving one bad
file among three leaves the other two processed and itself recorded in
`dir_walk()`'s outcome table; DESIGN's "GP6 unevenly met" known issue updated to
what is now contractual.

**Out:** the same treatment for the other probe-and-abort guards
(`os_check_config`, the `stopifnot(file.exists())` guards in `of_extract` /
`os_extract`) → ROADMAP candidate. The wider `stopifnot`→`cli` migration stays
opportunistic per DESIGN's Conventions. Argument-assembly changes → M13.

## Acceptance criteria

- [x] AC1 `ffp_count_streams()` on a path it cannot probe returns
      `c(Video = NA_integer_, Audio = NA_integer_)` and emits a
      `cli::cli_warn()` naming the file, instead of aborting. Evidence: two
      tests — a nonexistent path, and a path whose `ffprobe` call exits non-zero
      — each asserting both the returned value and the warning's message.
- [x] AC2 Each of the four call sites of `ffp_count_streams()` turns an `NA`
      count into a failure naming that file, and one test per site pins it:
      `os_check_audio` (`R/use_opensmile.R:112`) and `aw_check_audio`
      (`R/use_whisper.R:18`) return `FALSE` with a warning; `aw_prep_audio`
      (`R/use_whisper.R:102`) raises `cli::cli_abort()` so `dir_walk` records the
      row; `aw_transcribe` (`R/use_whisper.R:266`) already handles `NA` via its
      `is.na(has_audio)` branch and its test asserts that branch is reached
      rather than the `tryCatch` fallback.
- [x] AC3 An `aw_prep_audio_dir()` run over three inputs, one unprobeable,
      completes and returns a `dir_walk()` table (`R/utils.R:111-140`) whose row
      for the bad file has `success = FALSE` and a non-`NA` `error`, and whose
      other two rows have `success = TRUE`. Evidence: a mocked-boundary batch
      test asserting those three rows.
- [x] AC4 `cairn/DESIGN.md`'s "GP6 unevenly met" line (`:217-218`) is replaced by
      a dated line naming what is now contractual — a failed probe is a per-file
      outcome — and which guards remain ad hoc, listing them.
- [x] AC5 `devtools::test()` passes and `devtools::check()` reports 0 errors, 0
      warnings, and no note absent from a check of the default branch run the
      same day.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T5

## Tasks

- [x] T1 Rewrite `ffp_count_streams()` (`R/use_ffprobe.R:46-67`): drop the
      `stopifnot`, capture ffprobe's status, return `NA` counts with a
      `cli_warn`; tests first, both AC1 cases.
- [x] T2 Give `os_check_audio` and `aw_check_audio` their `NA` branch
      (`R/use_opensmile.R:112`, `R/use_whisper.R:18`) — `FALSE` plus a warning
      naming the file; tests first.
- [x] T3 Give `aw_prep_audio` its `NA` branch (`R/use_whisper.R:102`, replacing
      the `stopifnot` comparison that `NA` would otherwise poison) and pin
      `aw_transcribe`'s existing branch (`R/use_whisper.R:266`); tests first.
- [x] T4 Add the three-input batch test for AC3 against `aw_prep_audio_dir()`.
- [x] T5 Update DESIGN's GP6 known-issue line; `devtools::document()`,
      `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan chose a separate milestone depending on M13 over folding this into M13, because both rewrite ffp_count_streams — M13 its argument assembly, M14 its failure signal — and combined they cross the acceptance-criteria and task tripwires; falsified by the two edits proving inseparable in practice, i.e. M13 unable to convert R/use_ffprobe.R:51 without also deciding the NA contract.
- 2026-08-08: plan chose to split the NA disposition by caller — FALSE-plus-warning for the two check_audio predicates, cli_abort for aw_prep_audio — over one uniform disposition, because dir_walk records a row as failed only on an error (R/utils.R:118-127), so a warning-and-skip in aw_prep_audio would leave AC3's bad file reported as a success; falsified by a caller for which neither shape fits.
- 2026-08-08: T1 — `ffp_count_streams()` returns NA counts with a warning on a nonexistent file and on a non-zero ffprobe exit, read from the `status` attribute `system2(stdout = TRUE)` sets (measured, R 4.6.1); R's own status warning is muffled so one warning naming the file reaches the caller. A missing ffprobe still aborts (question gate).
- 2026-08-08: T2 — `os_check_audio()` and `aw_check_audio()` return FALSE on an NA count, before their second ffprobe query (which would fail on the same file); their own message is verbose-gated per the question gate, matching the sibling "no audio stream" warning. Added a `collect_warnings()` test helper: nested `expect_warning()` is order-dependent and misreported which of the two warnings was missing.
- 2026-08-08: T3 — `aw_prep_audio()` aborts naming the file on an NA count (dir_walk records a row as failed only on an error); `aw_transcribe()`'s existing `is.na(has_audio)` branch pinned by a test that discriminates it from the tryCatch fallback via the probe warning, verified by mutating the NA return to an abort and observing the test red on that assertion.
- 2026-08-08: T4 — batch test added. MEASURED against the pre-M14 sources: aw_prep_audio_dir() already survived the bad file (dir_walk caught the stopifnot), so AC3's success/error columns passed before the change; what M14 fixes on this path is the diagnosis — a failed probe was parsed as "0 audio streams" and reported as `(stream + 1) <= ffp_count_streams(infile)[["Audio"]] is not TRUE`, naming neither the file nor the reason. The test's message assertion is the discriminating one and is the only one that reds against main.
- 2026-08-08: T5 — DESIGN's GP6 known-issue line narrowed (guards still ad hoc listed from the stopifnot(file.exists()) sites in R/); NEWS entry added; document() no diff, test() 696 pass / 0 fail, check() 0 errors 0 warnings 0 notes.

- 2026-08-08: review returned M14 to in-progress (defect return 1). AC4 unticked: DESIGN's replacement line and the NEWS entry both claim a per-file failure disposition across all four callers, and `aw_transcribe_dir()` and `os_prep_audio_dir()` were measured returning `success = TRUE, error = NA` for an unprobeable file (A4 92, A17 88). Also actioned: A1 (93) the `had status` muffle is locale-dependent and leaks R's raw argv warning on a non-English R, measured in fr and de; A2 (85) the test pinning it is tautological; A3 (90) four assertions match cli-wrapped text and depend on temp-path length; A9 (87) `ffp_count_streams()` lost its scalar-input validation. Candidate row added for the batch-table success=TRUE gap.

- 2026-08-08: review fixes — A1/A2/B3: `ffp_count_streams()` now HOLDS every warning the ffprobe call raises and releases them only if the probe succeeded, so suppression keys on the exit status rather than on English message text; `fake_nonzero_exit()` defaults to the measured French wording and the test reds against the old grep (verified by restoring it). A3: `collect_warnings()` collapses cli's hard wraps and the four wrap-fragile assertions read through it. A9: a non-string `infile` aborts naming the argument, with tests for length-2, `character(0)` and a number. A18: `fixed = TRUE` on the basename matcher. A11 folded in via `!isTRUE(all(status == 0))`. A4/A17: DESIGN and NEWS rewritten to the measured per-batch reality — `aw_prep_audio_dir()` reports a failed row, `os_extract_dir()` a failed row naming a tempfile, `aw_transcribe_dir()` and `os_prep_audio_dir()` a success — plus a KNOWN GAP test pinning the NEWS limitation. DESIGN also now records that `run_tool()` reads no tool's exit status (A12) and ROADMAP's candidate row corrected: `os_extract` carries no file.exists guard (A20).

- 2026-08-08: self-caught regression in the A1 fix, before the fix-delta reviewer reported: holding every warning for the exit-status decision also held `find_program()`'s `set_program()` hint, which `ffp_count_streams()` then threw away as `require_program()`'s error unwound — a user with no ffprobe lost the one message telling them how to point openac at it. MEASURED (0 warnings reached the caller). Held conditions are now released in an error handler that re-raises, and the existing missing-tool test asserts the hint survives; verified by removing the release and observing that test red.

- 2026-08-08: fix-delta review (second round) — 10 findings, 5 actioned and fixed (F1, F4, F5, F8, F10), 3 more fixed though below the bar (F2, F6, and F4's AC4-note twin), 2 logged only (F3, F7, F9). F4 is a repeat of the defect class that returned this milestone: the corrected DESIGN paragraph asserted a message that does not exist. All six first-round fixes judged genuine. Suite 712 pass / 0 fail; check() 0/0/0.

## Decisions

## Review

_Verified 2026-08-08 on branch `m14-resilient-stream-count`, PR #17._

### Acceptance criteria

- AC1 — `devtools::test()` on `test-commands-probe.R`, three tests green.
  `ffp_count_streams() reports a nonexistent file rather than aborting`
  asserts `identical(streams, c(Video = NA_integer_, Audio = NA_integer_))`
  and the warning text, twice: once matching "does not exist", once matching
  the file's own basename. `ffp_count_streams() reports a failed probe rather
  than aborting` asserts the same NA vector for a probe exiting non-zero, with
  the warning matching "status 1". A third test,
  `a failed probe warns once, naming the file rather than the command`,
  collects every warning and asserts exactly one reaches the caller and that it
  names the file. Both AC1 cases return the exact vector the criterion names.

- AC2 — one test per call site, all green in the same run.
  `os_check_audio()` / `aw_check_audio()`: each has a pair —
  `returns FALSE on a file it cannot probe` asserts `FALSE` and that only ONE
  ffprobe call reached the boundary (the second query never issued), and
  `(verbose = TRUE) names the file it could not probe` asserts two warnings,
  both naming the file, the second being the check's own. The message is
  verbose-gated, matching these functions' existing "no audio stream" warning;
  the unconditional naming comes from `ffp_count_streams()` itself, so the
  criterion's "failure naming that file" holds at every verbosity.
  `aw_prep_audio()`: `aborts on a file it cannot probe, naming it` asserts
  `expect_error()` matching the file's basename and that ffmpeg was never
  reached — an abort, so `dir_walk()` records the row (AC3).
  `aw_transcribe()`: `reaches its NA branch, not its error fallback` asserts
  `NULL`, zero whisper calls, and exactly one warning matching
  "ffprobe exited with status 1". That warning is the discriminator: the
  `tryCatch` fallback yields the same skip message but emits no warning.
  Verified discriminating by mutation — replacing the NA return with an abort
  reds this test on the warning assertion (0 warnings) and on nothing else.

- AC3 — `one unprobeable file among three is a row, not the end of the batch`
  (`test-batch-dirs.R`), green. `aw_prep_audio_dir()` over `a.mp4`, `b.mp4`,
  `c.mp4` with `b`'s probe exiting non-zero returns three rows in that order:
  `success` is `c(TRUE, FALSE, TRUE)`, row 2's `error` matches
  "could not be counted", rows 1 and 3 have `NA` errors, and the boundary saw
  `ffprobe, ffmpeg, ffprobe, ffprobe, ffmpeg` — two conversions really happened
  and the bad file never reached ffmpeg. The criterion is met as written.
  Recorded so a later reader is not misled: MEASURED against the pre-M14
  sources, the `success`/`error` columns of this table ALREADY held — the batch
  did not die, because `dir_walk()` caught `aw_prep_audio()`'s `stopifnot`. The
  only assertion that reds against `main` is the error message: a failed probe
  was parsed as zero audio streams and reported as
  `(stream + 1) <= ffp_count_streams(infile)[["Audio"]] is not TRUE`, naming
  neither the file nor the reason and asserting something false about the input.
  AC3 verifies the table; what M14 changed on this path is the diagnosis in it.

- AC4 — `git diff main..HEAD -- cairn/DESIGN.md` shows the single
  "GP6 unevenly met" bullet replaced (the line moved to `:252` since the
  criterion was written). The replacement is dated
  `2026-07-11 (**narrowed 2026-08-08, M14**)`, states what is now contractual —
  a failed ffprobe is a per-file outcome, `NA` counts plus a warning naming the
  file, each of the four callers turning that into a per-file failure — and
  lists the guards that remain ad hoc: the `stopifnot(file.exists())` guards in
  `os_check_audio`, `os_prep_audio`, `os_extract_wav`, `os_fix_csv`,
  `aw_check_audio`, `aw_prep_audio`, `aw_transcribe_wav`, `of_extract`, plus
  `os_check_config()`. That list was derived from
  `grep -rn "stopifnot(file.exists\|os_check_config" R/` and each site resolved
  to its enclosing function, not composed from memory. `dir_outputs()`'s
  collision refusal is named as a deliberate pre-flight abort outside the set.
  No IP/GP principle text changed, so no impact report is owed.
  **Corrected 2026-08-08, same review:** this evidence line was wrong about the
  substance of the replacement. The line is dated and does list the ad-hoc
  guards, but what it names as contractual — "each of its four callers turns
  that into a per-file failure, so a `*_dir()` batch records the bad file" — is
  false for `aw_transcribe_dir()` and `os_prep_audio_dir()` (A4, A17, both
  measured). AC4 is unticked and the criterion is unmet.

- AC5 — `devtools::test()`: 696 passing, 0 failing, 6 skipped (all pre-existing
  opt-in gates: four `OPENAC_INSTALLER_RUN` installer tests, one OpenFace-absent,
  one audio.whisper-absent). `devtools::check()`: **Status: OK** — 0 errors,
  0 warnings, 0 notes. With zero notes on the branch, the criterion's
  "no note absent from a check of the default branch" is satisfied with nothing
  to compare against.

### Consistency gate

- `cairn_validate` exit 0 — every CHECK PASS, every advisory OK.
- `cairn_impact` not owed: the diff changes DESIGN's Known-issues section only,
  not an IP/GP principle.
- Toolchain (`r-package` profile): `devtools::document()` leaves no diff;
  `NAMESPACE`/`man/` regenerate rather than hand-edited; README.Rmd and
  README.md untouched by this branch and in sync; no `_pkgdown.yml` in the repo,
  so that check no-ops; `NEWS.md` carries an entry for the user-visible change,
  with no milestone number in it; no new top-level files, so no
  `.Rbuildignore` entry owed; full `check()` clean as recorded under AC5.

### Independent review (2026-08-08)

Three fresh-context lenses (diff-bug [O], blame-history [S], prior-PR-comments
[S]) reported 33 candidate findings; a fresh [S] scorer scored each. Seven
scored >= 80 and are actioned; 26 scored below 80 and are logged below, not
discarded.

**Actioned (>= 80):**

- A1 (93) — the `had status` muffle in `ffp_count_streams()` matches literal
  English text, but R's own `system2` status warning is translated. MEASURED
  independently at review: `LANGUAGE=fr` gives "l'execution de la commande '...'
  renvoie un statut 1" and `LANGUAGE=de` "Ausfuehrung von Kommando '...' ergab
  Status 1"; neither contains `had status`, so a non-English user gets BOTH
  warnings, including the raw argv dump the muffle exists to suppress. B3 (90)
  is the same defect from the history lens.
- A4 (92) — `aw_transcribe_dir()` records an unprobeable file as
  `success = TRUE, error = NA`: `aw_transcribe()` skips via
  `cli_alert_warning()` (a message, not a condition `dir_walk()` records) and
  returns NULL. MEASURED independently at review. Same substance as M07 review's
  D11 (55), still open.
- A17 (88) — the NEWS sentence "In a batch, the file appears in the returned
  table as a failure you can read and re-run" and DESIGN's "each of its four
  callers turns that into a per-file failure" are false for two of the four
  batch entry points. MEASURED at review: `os_prep_audio_dir()` also returns
  `success = TRUE, error = NA` with no wav written. Branch-added prose asserting
  behavior that was never derived from an execution.
- A3 (90) — four message assertions match text that `cli_warn()` hard-wraps, so
  they depend on the infile path length and console width; a 19-character path
  at width 80 reds them. They pass today only because macOS `tempdir()` is long.
- A9 (87) — `ffp_count_streams()` lost its only input validation: a length-2
  `infile` now errors "the condition has length > 1" and `character(0)` errors
  "argument is of length zero", where `stopifnot(logical(0))` passed vacuously.
- A2 (85) — `fake_nonzero_exit()` hand-writes the English "had status" string
  the muffle greps for, so the "warns once" test is tautological with respect to
  the property it claims to pin and is green in every locale.

**Logged, below the action bar (26):** A5 (35) `aw_transcribe`'s collapsed skip
message, pre-existing · A6 (55) the NA-branch test's discriminator is a proxy ·
A7 (45) `os_prep_audio_dir` success=TRUE, pre-existing path · A8 (38)
`os_extract_dir`'s tempfile-naming error, pre-existing · A10 (35) NA return is a
caller hazard — the contract AC1 specifies · A11 (65) status-attribute edge
cases (character, length>1) · A12 (30) no exit-status check for the other tools,
Scope Out · A13 (32) second ffprobe call unmuffled, unmodified code · A14 (22)
`os_check_audio`'s `dat[[3]]` unguarded, pre-existing · A15 (62) / B2 (62) the
nonexistent-file branch is unreachable from three callers · A16 (40) three NA
predicates for one contract · A18 (68) `expect_error(..., basename(infile))`
treats a filename as a regex · A19 (20) batch test couples to future's plan ·
A20 (40) DESIGN's "Calling the CLIs" omits the status attribute; Scope Out
misnames `os_extract`'s guard · B1 (25), B4 (15), B5 (15), B7 (15) no-conflict
reports · B6 (52) no test asserts the success-path return type · C0 (10) and
five C bullets (15, 60, 10, 10, 10), the prior-review lens finding zero
regressions.

### Disposition — returned to implement

Two actioned findings cross the return floor:
- A1 (93) is a defect in what the package does for its users, in a mechanism
  this branch introduced.
- A17 (88) with A4 (92) falsifies **AC4**: the criterion requires DESIGN's line
  to name "what is now contractual", and the line as written names a per-file
  failure disposition across all four callers that two of the four do not have.
  The criterion fails inside its own domain, so this is a defect return, not an
  amendment return.

Status back to `in-progress`. Defect returns for M14: 1.

### AC4 re-verified (2026-08-08, after the review fixes)

- AC4 — `git diff main..HEAD -- cairn/DESIGN.md` shows the "GP6 unevenly met"
  bullet replaced by a dated line whose every claim was measured rather than
  inferred. What it names as contractual is now `ffp_count_streams()` and its
  four callers' NA dispositions — which is what the branch actually delivers —
  and it states the batch-level reality separately, per entry point:
  `aw_prep_audio_dir()` a failed row naming the file; `os_extract_dir()` a
  failed row whose message is the bare `stopifnot()` deparse
  `file.exists(infile) is not TRUE`, naming no file at all
  (**corrected 2026-08-08, fix-delta review F4:** this line first said the
  message "names a tempfile the caller never passed" — it names nothing; that
  described what the failure is ABOUT rather than what the table records, which
  is the same unverified-claim defect the milestone was returned on, written a
  second time in the fix for it. The table was read; the sentence then said
  something the read did not support);
  `aw_transcribe_dir()` and `os_prep_audio_dir()` a **success**. Each of those
  four was executed against the mocked boundary at review and read off the
  returned table, not derived from reading the callers. The ad-hoc guards are
  listed as before, now with `run_tool()`'s unread exit status named as the
  larger remaining gap. NEWS carries the same limitation in user-facing words,
  and a `KNOWN GAP` test in `test-batch-dirs.R` fails if `os_prep_audio_dir()`
  ever stops recording a skipped file as a success, so the changelog claim is
  enforced rather than asserted.

### Fix-delta review (2026-08-08, second round)

A fresh [O] reviewer read the fix commits; a fresh [S] scorer scored the ten new
findings. It judged all six first-round fixes genuine and independently
reproduced the regression the session had already self-caught and fixed
(held warnings swallowed on the error path).

**Actioned (>= 80):** F1 (92) a warning raised alongside a FAILED probe was
dropped with R's status report — suppression now takes only the LAST held
warning, on the measured premise that R signals the status last, and a test
pins that a diagnostic raised beside a rejected file survives · F4 (90) DESIGN
claimed `os_extract_dir()`'s message "names a tempfile"; the measured message is
`file.exists(infile) is not TRUE`, which names nothing — corrected in DESIGN and
in the AC4 note, which had repeated it · F10 (85) NEWS called
`os_prep_audio_dir()`'s outcome a skip; it runs ffmpeg and never checks the exit
status — reworded · F5 (82) the KNOWN GAP test covered one of the two entry
points NEWS names — now covers both · F8 (80) the new scalar-`infile` guard
tightened an exported contract undocumented — NEWS and `@return` now say so.

**Logged, below the bar (5):** F2 (45) `integer(0)` status read as success —
fixed anyway, one comparison · F3 (30) `stop(e)` re-raises from an unwound
frame, shortening `traceback()` · F6 (60) the KNOWN GAP test reds under only one
of the two contemplated fixes — addressed anyway via
`dir_walk_reports_failure()` · F7 (75) `aw_transcribe()`'s pre-existing
`tryCatch` swallows the new input-validation abort and reports it as
"No audio streams detected"; reachable only through an already-invalid
non-string `infile`, left to the candidate row · F9 (25) the whitespace collapse
could theoretically join two cli bullets.

F4 is the finding that matters most for what this milestone learned: the fix for
an unverified-prose defect introduced another one, in the same paragraph. Both
times the sentence described what the failure was ABOUT rather than what was
actually observed.

### Final verification (2026-08-08, after both fix rounds)

All five criteria re-executed against the final tree, since the code changed
after the first evidence pass. AC1/AC2/AC3 test files: `test-commands-probe.R`
25 tests / 79 assertions, `test-commands-prep.R` 14 / 26,
`test-whisper-transcribe.R` 14 / 47, `test-batch-dirs.R` 18 / 58 — 0 failures.
AC4 re-verified against the twice-corrected DESIGN line, every claim in it read
off an executed batch table. AC5: `devtools::test()` 712 passing / 0 failing /
6 pre-existing opt-in skips; `devtools::check()` 0 errors, 0 warnings, 0 notes;
`cairn_validate` exit 0; `document()` no diff. CI green on all five jobs
(ubuntu release/devel/oldrel-1, macOS, Windows) at `59335a6`.

Process note: the first review round returned the milestone to `in-progress`
(defect return 1). The second round's findings were fixed with the milestone
left at `review` rather than cycling the status again — the fixing was
continuous with the user's instruction to fix the actioned list, and no
criterion was unticked in that round except by the F4 correction, which was
repaired before the gate. Recorded rather than smoothed over: a strict reading
sets `in-progress` for F1 as well, on its score alone.
