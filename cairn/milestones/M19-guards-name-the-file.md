# M19: A guard that names no file

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** `m19-guards-name-the-file`

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

- [x] T1 Test-first, red before the change: the missing-intermediate test for
      `os_extract_dir()` (AC2), and one message test per guard T2 enumerates
      (AC1).
- [x] T2 Enumerate the guards to rewrite from
      `grep -n "stopifnot\|cli_abort" R/use_*.R`, keeping those reachable from
      `dir_walk()`; record the list and each disposition in the work log.
- [x] T3 Rewrite those guards as `cli_abort()` messages naming the file and
      the defect, `os_fix_csv()` included.
- [x] T4 Give `os_check_config()` a message naming the unresolved config
      value.
- [x] T5 Move `os_extract_dir()`'s `config` validation pre-flight, beside its
      existing argument checks (`R/use_opensmile.R:419-425`).
- [x] T6 Update the tests that pin the old deparse text —
      `tests/testthat/test-commands-extract.R:118`, `:176`;
      `test-commands-prep.R:112`, `:148`, `:213`;
      `test-whisper-transcribe.R:134`, `:161`.
- [ ] T7 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned four findings on this file — AC2's premise unreachable (`dir_inputs()` enumerates from `list.files()`, so every batch input exists by construction) and unsatisfiable for `aw_transcribe_dir()`, which has no existence guard; AC1's grep enumerating one syntactic form while its prose universal contradicted Scope's reader-guard exemption; AC3's "aborts returning no rows" describing two different behaviors and leaving the default `config` path unpinned; all four fixed before the gate, none deferred.
- 2026-08-08: plan gate chose retargeting AC2 to the missing-INTERMEDIATE case over the missing-input case because the latter is unreachable through any `*_dir()` wrapper and the former is the defect DESIGN measured on 2026-08-08; falsified by a batch path that can enumerate an input which then disappears before use.
- 2026-08-08: plan gate chose validating `config` pre-flight over recording it as N per-file failures because a batch-wide argument error is not a per-file outcome and the per-file form costs N ffprobe rounds before failing; falsified by a config that legitimately varies per file.
- 2026-08-09: implement gate chose (a) rewriting every guard inside a per-file function, argument-type checks included, over only the file-property ones — Scope's "every other one is a bare deparse" is the literal domain, and a batch given a bad `aus =` still records N rows reading `is_bool(aus) is not TRUE`; (b) one shared internal helper building every message in `run_checked()`'s established shape over 38 bespoke blocks, for consistency and one condition class to test on; (c) reading `os_extract_dir()`'s pre-flight `config` default from `formals(os_extract)` over repeating the literal, so the pre-flight check and the per-file call cannot disagree.
- 2026-08-09: T2 guard enumeration, input `grep -n "stopifnot\|cli_abort" R/use_*.R` (74 hits). Batch-reachable — inside a function `dir_walk()` calls once per file — 38 guards: `os_check_audio` `use_opensmile.R:114-115`, `os_prep_audio` `:184-187`, `os_extract_wav` `:365-369`, `os_fix_csv` `:491`, `of_extract` `use_openface.R:75-84`, `aw_check_audio` `use_whisper.R:15-16`, `aw_prep_audio` `:110-114`/`:134`/`:139`, `aw_transcribe_wav` `:388-395`, `aw_transcribe` `:319`; the last two of those (`use_whisper.R:134`, `:319`) already name the file and need only a test. Not batch-reachable, each with its reason: the `*_dir()` pre-flight guards (`use_opensmile.R:259-262`, `:453-459`, `use_openface.R:146-149`, `use_whisper.R:230-233`, `:488-493`) abort before `dir_walk()` is entered, so no row exists to carry their message; `os_check_config` `use_opensmile.R:86`, `:91` stops being reachable at T5, which validates `config` pre-flight, and AC3 governs its message instead; `ffp_count_streams` `use_ffprobe.R:68` rejects a value that is not a file path, which `dir_walk()`'s `infile` column (always length-1 character from `fs::path_abs()`) cannot be, and has no file to name; the reader guards `os_read` `use_opensmile.R:530-539`, `of_read` `use_openface.R:193-202`, `aw_read_data` `use_whisper.R:575-604` are outside the batch path and excluded by Scope.
- 2026-08-09: T1 wrote `tests/testthat/test-guard-messages.R` (43 tests) before any source change and MEASURED it red: 41 failed, 2 passed. The two that passed are exactly the two guards T2 recorded as already naming their file (`use_whisper.R:134`, `:319`), so the suite discriminates on the property under test rather than on the guards' presence; every other failure reads the bare deparse it exists to remove (e.g. `x | file.exists(infile) is not TRUE`).
- 2026-08-09: T3/T4/T5/T6 landed in ONE commit rather than four, because the seven surviving tests T6 names pin the very `stopifnot()` deparse text T3 replaces (`"file.exists"`, `"is_string"`, `"is_bool"`, `"file_ext"`, `"Config file not found"`, `"aw_check_audio"`, `"Audio"`), so no ordering of the four leaves `devtools::test()` clean at an intermediate checkpoint; the profile's verify slot is clean at the commit that exists.
- 2026-08-09: minor amendment, T3 — `aw_transcribe_wav()` gained a `source` argument mirroring `os_extract_wav()`'s (M17 review, finding B). Without it its missing-`infile` guard names the temporary wav `aw_transcribe()` derived rather than the file the user chose, which is the same defect AC2 removes on the openSMILE side; the argument is internal and defaults to `infile`, so a direct call is unchanged.
- 2026-08-09: minor amendment, T3 — `aw_transcribe_wav()`'s model guard moved from `class(model) == "whisper"` to `inherits()`. The old form compares a whole class vector against one string, so a subclassed model made `stopifnot()` die on the comparison's length rather than on the contract.
- 2026-08-09: T6 removed three tests outright rather than retargeting them — `os_prep_audio()`/`aw_prep_audio()`/`of_extract()` "validates its arguments" — each of whose entire content was a deparse pin now asserted guard-by-guard, and more strongly, in `test-guard-messages.R`; a pointer comment stands where each was. The four with assertions beyond the message (whisper never reached, the stream index, the config) were retargeted in place.

## Decisions

## Review
