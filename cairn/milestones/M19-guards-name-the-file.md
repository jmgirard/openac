# M19: A guard that names no file

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** —

## Goal

Make every input-file guard in the batch path say which file it stopped on and
what was wrong with it, and validate the batch-wide arguments once before the
loop rather than once per file.

## Scope

**In:** A batch row's `error` column carries the guard's message, and in the
per-file path exactly one guard names the file it stopped on
(`R/use_whisper.R:123`). Every other one is a bare `stopifnot()` deparse —
`file.exists(infile) is not TRUE` — so the row's message names neither the
file nor the defect; the file name reaches the user only because `dir_walk()`
prepends it (`R/utils.R:125`). Those guards become `cli_abort()` messages
naming the file and the defect (GP9). `os_check_config()`
(`R/use_opensmile.R:86-91`) names the config it could not resolve, and
`os_extract_dir()` validates `config` pre-flight instead of once per file
inside the loop, where a typo costs N ffprobe rounds and N identical anonymous
warnings. `os_fix_csv()` (`R/use_opensmile.R:457`) attributes its missing input
to openSMILE having written no output. The tests pinning the old deparse text
move with them.

**Out:** Non-zero tool exits → M17. The skip state → M18. Output-path
collisions → the standing ROADMAP candidate, behind M18. Guards outside the
batch path — the readers, `set_program()`, the installers — keep their current
messages; this milestone is scoped to what a batch row can carry.

## Acceptance criteria

- [ ] AC1 Every guard T2's work-log enumeration classifies as batch-reachable
      signals an error whose message names the file it stopped on and the
      defect, and a test per guard asserts the file's basename appears in it.
      That enumeration is the domain and is stated rather than assumed: its
      input is `grep -n "stopifnot\|cli_abort" R/use_*.R`, and every hit T2
      classifies as not batch-reachable carries its reason.
- [ ] AC2 The defect DESIGN's Known issues measured on 2026-08-08 is gone:
      `os_extract_dir()` recording a failed row whose message is the bare
      deparse `file.exists(infile) is not TRUE`, about a temporary wav that
      was never written. A test drives that path and asserts the row's `error`
      names the missing wav path and attributes it to ffmpeg having written no
      output there.
- [ ] AC3 `os_check_config()` signals an error whose message contains the
      config value it could not resolve, and `os_extract_dir()` validates
      `config` before `dir_walk()` runs. Tests assert: the value appears in
      the message; an unresolvable config makes `os_extract_dir()` signal an
      error rather than return a table; `boundary_tools()` records no call at
      all (`helper-openac.R:925`), not merely no openSMILE call; and the same
      validation runs when `config` is left at the default carried by
      `os_extract_wav()`'s signature rather than passed through `...`.
- [ ] AC4 `os_fix_csv()`'s missing-input guard names the path it looked for
      and attributes it to openSMILE having written no output there; a test
      asserts both parts. The attribution is honest for both of its callers
      (`R/use_opensmile.R:361`, `:364`), which is what makes it sayable.
- [ ] AC5 `devtools::document()` shows no drift, `devtools::test()` passes,
      and `devtools::check()` reports 0 errors, 0 warnings and no NOTE other
      than the pre-existing `spelling` NOTE.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T3
- AC3 → T4, T5
- AC4 → T3, T6
- AC5 → T7

## Tasks

- [ ] T1 Test-first, red before the change: the missing-intermediate test for
      `os_extract_dir()` (AC2), and one message test per guard T2 enumerates
      (AC1).
- [ ] T2 Enumerate the guards to rewrite from
      `grep -n "stopifnot\|cli_abort" R/use_*.R`, keeping those reachable from
      `dir_walk()`; record the list and each disposition in the work log.
- [ ] T3 Rewrite those guards as `cli_abort()` messages naming the file and
      the defect, `os_fix_csv()` included.
- [ ] T4 Give `os_check_config()` a message naming the unresolved config
      value.
- [ ] T5 Move `os_extract_dir()`'s `config` validation pre-flight, beside its
      existing argument checks (`R/use_opensmile.R:419-425`).
- [ ] T6 Update the tests that pin the old deparse text —
      `tests/testthat/test-commands-extract.R:118`, `:176`;
      `test-commands-prep.R:112`, `:148`, `:213`;
      `test-whisper-transcribe.R:134`, `:161`.
- [ ] T7 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned four findings on this file — AC2's premise unreachable (`dir_inputs()` enumerates from `list.files()`, so every batch input exists by construction) and unsatisfiable for `aw_transcribe_dir()`, which has no existence guard; AC1's grep enumerating one syntactic form while its prose universal contradicted Scope's reader-guard exemption; AC3's "aborts returning no rows" describing two different behaviors and leaving the default `config` path unpinned; all four fixed before the gate, none deferred.
- 2026-08-08: plan gate chose retargeting AC2 to the missing-INTERMEDIATE case over the missing-input case because the latter is unreachable through any `*_dir()` wrapper and the former is the defect DESIGN measured on 2026-08-08; falsified by a batch path that can enumerate an input which then disappears before use.
- 2026-08-08: plan gate chose validating `config` pre-flight over recording it as N per-file failures because a batch-wide argument error is not a per-file outcome and the per-file form costs N ffprobe rounds before failing; falsified by a config that legitimately varies per file.

## Decisions

## Review
