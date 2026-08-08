# M14: A bad file is an outcome, not the end of the batch

- **Status:** planned
- **Priority:** normal
- **Depends on:** M13
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** —

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

- [ ] AC1 `ffp_count_streams()` on a path it cannot probe returns
      `c(Video = NA_integer_, Audio = NA_integer_)` and emits a
      `cli::cli_warn()` naming the file, instead of aborting. Evidence: two
      tests — a nonexistent path, and a path whose `ffprobe` call exits non-zero
      — each asserting both the returned value and the warning's message.
- [ ] AC2 Each of the four call sites of `ffp_count_streams()` turns an `NA`
      count into a failure naming that file, and one test per site pins it:
      `os_check_audio` (`R/use_opensmile.R:112`) and `aw_check_audio`
      (`R/use_whisper.R:18`) return `FALSE` with a warning; `aw_prep_audio`
      (`R/use_whisper.R:102`) raises `cli::cli_abort()` so `dir_walk` records the
      row; `aw_transcribe` (`R/use_whisper.R:266`) already handles `NA` via its
      `is.na(has_audio)` branch and its test asserts that branch is reached
      rather than the `tryCatch` fallback.
- [ ] AC3 An `aw_prep_audio_dir()` run over three inputs, one unprobeable,
      completes and returns a `dir_walk()` table (`R/utils.R:111-140`) whose row
      for the bad file has `success = FALSE` and a non-`NA` `error`, and whose
      other two rows have `success = TRUE`. Evidence: a mocked-boundary batch
      test asserting those three rows.
- [ ] AC4 `cairn/DESIGN.md`'s "GP6 unevenly met" line (`:217-218`) is replaced by
      a dated line naming what is now contractual — a failed probe is a per-file
      outcome — and which guards remain ad hoc, listing them.
- [ ] AC5 `devtools::test()` passes and `devtools::check()` reports 0 errors, 0
      warnings, and no note absent from a check of the default branch run the
      same day.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T5

## Tasks

- [ ] T1 Rewrite `ffp_count_streams()` (`R/use_ffprobe.R:46-67`): drop the
      `stopifnot`, capture ffprobe's status, return `NA` counts with a
      `cli_warn`; tests first, both AC1 cases.
- [ ] T2 Give `os_check_audio` and `aw_check_audio` their `NA` branch
      (`R/use_opensmile.R:112`, `R/use_whisper.R:18`) — `FALSE` plus a warning
      naming the file; tests first.
- [ ] T3 Give `aw_prep_audio` its `NA` branch (`R/use_whisper.R:102`, replacing
      the `stopifnot` comparison that `NA` would otherwise poison) and pin
      `aw_transcribe`'s existing branch (`R/use_whisper.R:266`); tests first.
- [ ] T4 Add the three-input batch test for AC3 against `aw_prep_audio_dir()`.
- [ ] T5 Update DESIGN's GP6 known-issue line; `devtools::document()`,
      `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan chose a separate milestone depending on M13 over folding this into M13, because both rewrite ffp_count_streams — M13 its argument assembly, M14 its failure signal — and combined they cross the acceptance-criteria and task tripwires; falsified by the two edits proving inseparable in practice, i.e. M13 unable to convert R/use_ffprobe.R:51 without also deciding the NA contract.
- 2026-08-08: plan chose to split the NA disposition by caller — FALSE-plus-warning for the two check_audio predicates, cli_abort for aw_prep_audio — over one uniform disposition, because dir_walk records a row as failed only on an error (R/utils.R:118-127), so a warning-and-skip in aw_prep_audio would leave AC3's bad file reported as a success; falsified by a caller for which neither shape fits.

## Decisions

## Review
