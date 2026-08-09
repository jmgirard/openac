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
