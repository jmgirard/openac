# M19: A guard that names no file

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** `m19-guards-name-the-file` · https://github.com/jmgirard/openac/pull/20

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

- [x] AC1 Every guard T2's work-log enumeration classifies as batch-reachable
      signals an error whose message names the file it stopped on and the
      defect, and a test per guard asserts the file's basename appears in it.
      That enumeration is the domain and is stated rather than assumed: its
      input is `grep -n "stopifnot\|cli_abort" R/use_*.R`, and every hit T2
      classifies as not batch-reachable carries its reason.
- [x] AC2 The defect DESIGN's Known issues measured on 2026-08-08 is gone:
      `os_extract_dir()` recording a failed row whose message is the bare
      deparse `file.exists(infile) is not TRUE`, about a temporary wav that
      was never written. A test drives that path and asserts the row's `error`
      names the missing wav path and attributes it to ffmpeg having written no
      output there.
- [x] AC3 `os_check_config()` signals an error whose message contains the
      config value it could not resolve, and `os_extract_dir()` validates
      `config` before `dir_walk()` runs. Tests assert: the value appears in
      the message; an unresolvable config makes `os_extract_dir()` signal an
      error rather than return a table; `boundary_tools()` records no call at
      all (`helper-openac.R:925`), not merely no openSMILE call; and the same
      validation runs when `config` is left at the default carried by
      `os_extract_wav()`'s signature rather than passed through `...`.
- [x] AC4 `os_fix_csv()`'s missing-input guard names the path it looked for
      and attributes it to openSMILE having written no output there; a test
      asserts both parts. The attribution is honest for both of its callers
      (`R/use_opensmile.R:361`, `:364`), which is what makes it sayable.
- [x] AC5 `devtools::document()` shows no drift, `devtools::test()` passes,
      and `devtools::check()` reports 0 errors, 0 warnings and no NOTE other
      than the pre-existing `spelling` NOTE.

## Coverage

- AC1 → T1, T2, T3, T8
- AC2 → T1, T3
- AC3 → T4, T5, T9
- AC4 → T3, T6, T11
- AC5 → T7, T10, T11, T12

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
- [x] T7 `devtools::document()`, `devtools::test()`, `devtools::check()`.
- [x] T8 Review round 1, AC1: no batch-reachable guard raises a raw R
      condition — the typed-`NA` `stream`, the non-scalar `infile`, and the
      `NA` field from the second ffprobe query (F1, F2, F3).
- [x] T9 Review round 1, AC3: the pre-flight `config` check reads the argument
      the call will read — a partially-matched name, an explicit `NULL`, and an
      unresolved openSMILE (F5, F6, F12).
- [x] T10 Review round 1, actioned beyond the criteria: the `error` column is
      one unglyphed line naming the file once (F14), and the DESIGN/NEWS
      claims narrow to what the branch does (F8).
- [x] T11 Review round 2, F1/F2/F3: the `error` column carries plain data —
      `os_fix_csv()` routed through `abort_file()` so round 1's fix reaches it,
      no ANSI escapes baked in by the eager format, and the property asserted
      over every guard case on the RAW message rather than the collapsed one.
- [x] T12 Review round 2, F6 (and F5, below the bar, same helper):
      `match_formals()` rejects two prefixes of one formal instead of swallowing
      the second, and does not partial-match formals declared after `...`.

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
- 2026-08-09: discovered sub-task (minor amendment): a NEWS entry for the two user-visible changes, and a DESIGN Known-issues correction in place — its GP6 paragraph asserted "the abort messages name the file in only one of them, the rest being bare `stopifnot()` deparses", which the branch makes false. The NEWS claim that the config message points at `os_list_configs()` was untested when written, so an assertion for it was added rather than the claim softened.
- 2026-08-09: T7 MEASURED on R 4.6.1 / macOS 15 at commit before this one: `devtools::document()` writes no diff, `devtools::test()` 308 tests 0 failures 0 errors 6 skips, `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes; the pre-existing `spelling` NOTE AC5 allows for did not appear at all.
- 2026-08-09: T7 also added `os_extract_dir()`'s `@return` note that an unresolvable `config` errors before any file is touched and returns no table — the abort T5 introduces was absent from its own documented contract, which is the shape of finding the M18 review logged against `aw_transcribe()`.
- 2026-08-09: status → review.
- 2026-08-09: review round 1 returned the milestone to `in-progress` (defect return 1). What failed: AC1, falsified by three guards that raise a raw R condition instead of reaching `abort_file()` — a typed `NA_integer_` `stream` (`use_opensmile.R:213`, `use_whisper.R:122`), a non-scalar `infile` (`use_opensmile.R:131`, `:206`, `use_whisper.R:115`, `use_openface.R:75`), and an `NA` field from the SECOND ffprobe query reaching `if (!os_check_audio(infile))` (`use_opensmile.R:410`, `use_whisper.R:422`); and AC3, falsified by `do.call()`'s partial matching letting `conf =` bypass the pre-flight check entirely (`use_opensmile.R:529`), by an unresolved openSMILE killing the whole batch on `dirname(NULL)` before `os_check_config()` can speak (`:531`), and by `config = NULL` pre-flighting the default instead. Also actioned: DESIGN/NEWS overclaim three behaviors the branch lacks, and `dir_walk()`'s `error` column now carries wrapped newlines, a glyph, and the filename twice. AC2, AC4, AC5 verified and unaffected.
- 2026-08-09: minor amendment — T8, T9, T10 added for the round-1 return, one per group of findings; the Coverage map gains T8 under AC1, T9 under AC3, and T10 under AC5. No acceptance criterion's text changes: round 1 was a defect return, and both criteria failed as written.
- 2026-08-09: T8 wrote 25 tests for F1/F2/F3 before any source change and MEASURED them red, each reproducing its finding's own condition rather than a bare failure — `the condition has length > 1`, `argument is of length zero`, `invalid 'file' argument`, `subscript out of bounds`. Fixes: `is.na(stream)` ahead of the `stream < 0` comparison (`is_integerish(NA_integer_, n = 1)` is TRUE, so a TYPED NA reached it); a shared `check_file_arg()` run before any guard that would interpolate the path, in all nine per-file entry points plus `ffp_count_streams()`, whose hand-written twin of it moved into the helper; and `isTRUE(all(tests))` in both `*_check_audio()`, plus the `length(dat) < 3` branch `aw_check_audio()` already had and `os_check_audio()` did not.
- 2026-08-09: T8 disposition — `check_file_arg()` names the ARGUMENT, not a file, and is classified NOT batch-reachable for the reason T2 gave `ffp_count_streams()`'s identical guard: `dir_walk()`'s `infile` column is always a length-1 character from `fs::path_abs()`. It cannot honestly name a file: `basename()` of a length-2 path names two and of `character(0)` names none, which is F11's below-bar defect removed by the same guard.
- 2026-08-09: T9 wrote three tests red, then fixed F5 with `match_formals()` (resolves an unambiguous prefix the way `do.call()` will, so `conf =` is checked rather than silently reaching `config`), F12 by testing `"config" %in% names(extra_args)` instead of `is.null(config)` (supplied means checked, whatever the value), and F6 by routing `os_list_configs()` and `os_check_config()` through `require_program("opensmile")` rather than `find_opensmile()`, whose NULL made `dirname(NULL)` kill the batch. All three assert `boundary_tools()` is `character(0)` or that the message names openSMILE, so the pre-flight is shown to hold before any tool runs.
- 2026-08-09: T10 F14 — `abort_file()` now formats its message eagerly with `cli::format_inline()` and signals it through `rlang::abort()`. cli formats LAZILY and for a terminal, so a `cli_abort()` template was still being wrapped at the console width and given an "x" glyph when `conditionMessage()` ran, and setting `cli.width` around the call could not reach it. MEASURED 2026-08-09 on R 4.6.1 / cli 3.6.6: the old form returned `"Could not process 'clip.mp4'.\n<glyph> No file exists at '/nope/clip.mp4'."`; it now returns that text on one line, joined by a colon. The condition also carries `defect` — the message without the leading file — which `dir_walk()`'s warning uses so the basename it already prints is not printed twice.
- 2026-08-09: T10 F8 — the DESIGN Known-issues sentence and the NEWS entries narrowed to what the branch does. Two claims were false and are gone: that EVERY per-file guard routes through `abort_file()` (`os_fix_csv()` hand-rolls one, and `check_file_arg()` names no file), and that `config` is resolved once rather than per file (`os_extract_wav()` still resolves it, so the count is N+1). The third, that a bad `config` costs no ffprobe rounds, was false only under F5 and is now true and asserted.
- 2026-08-09: T8-T10 verify slot MEASURED on R 4.6.1 / macOS 15: `devtools::document()` writes no diff, `devtools::test()` 1009 tests 0 failures 0 errors 6 skips, `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes. The round's own delta was measured by stashing it: the same command at commit 882df74 reports 928 passing, so this round adds 81. That 928 supersedes the `308` the T7 line above records for the same command — T7's figure is not reproducible at the commit it names and the procedure behind it is unknown, so it is superseded rather than relied on.
- 2026-08-09: review round 2 returned the milestone to `in-progress` (defect return 2). No acceptance criterion failed — all five verified with fresh evidence — but two findings scored >= 90 on user-facing defects: `os_fix_csv()` hand-rolls its abort, so round 1's F14 fix never reached the one guard whose message still carries a newline and a bullet glyph into the `error` column (falsifying NEWS, DESIGN and the AC4 evidence line the branch itself added), and `match_formals()` turns R's duplicate-argument error into silence, swallowing a second supplied argument into `...`. Also actioned: `abort_file()` bakes ANSI colour codes into the same column, and no test could have caught any of it because `collapsed_guard()` deletes the newline under test.
- 2026-08-09: minor amendment — T11 and T12 added for the round-2 return, grouped by fix rather than by finding: F1, F2 and F3 are one property of the `error` column (plain data, not console output) and share a test, F6 and F5 are one helper. The Coverage map gains T11 under AC4 (it rewrites the message AC4 reads) and T11/T12 under AC5. No acceptance criterion's text changes: round 2 falsified none.
- 2026-08-09: implement gate chose (a) routing `os_fix_csv()` through `abort_file()` and letting its wording become the shared shape over keeping the bespoke wording with a second copy of the flattening, because a guard that opts out of the helper is exactly how F14's fix missed it; (b) rejecting a duplicate-prefix argument once, pre-flight, over leaving it for `do.call()` to reject per file, on the plan gate's own reasoning that a batch-wide argument error is not a per-file outcome; (c) fixing F5's post-`...` partial match in the same pass, three lines inside the helper already being rewritten.
- 2026-08-09: T11 wrote the property test first over the same `guard_cases()` table the collapsed assertions read, at `cli.width = 40` and `cli.num_colors = 256` so a console-formatted message must misbehave, and MEASURED it red on both counts. F1 reproduced verbatim at those settings: `os_fix_csv()`'s `conditionMessage()` returned `"Could not tidy the openSMILE\noutput at\n'...clip.csv'.\n<glyph> openSMILE wrote no output there."` F3 reproduced across every `abort_file()` case as `\033[34m`-wrapped paths. Fixes: `os_fix_csv()` routed through `abort_file()` (its message becomes the shared shape, and AC4's two parts — the full path and the openSMILE attribution — survive the rewording, which is what its existing test asserts); `cli::ansi_strip()` as the last step of both formats in `abort_file()`, unconditional rather than an option set around the call. The round-1 F14 test gained the colour assertion it lacked.
- 2026-08-09: T11 also corrected DESIGN's Known-issues sentence in place: it named `os_fix_csv()` as one of two guards outside `abort_file()`, which this task makes false, and omitted `os_check_config()`, which the round-2 review logged below the bar (F12) as the real third. It now names the two that are actually outside and why each is.
- 2026-08-09: T12 wrote three tests red first. The duplicate-prefix one was rewritten before it counted as red: `conf` and `confi` are both substrings of `config`, so the obvious `expect_match(msg, "confi")` passes on any message naming the formal and discriminates nothing — the assertions read backticked `` `conf` ``/`` `confi` `` instead. Cause MEASURED on R 4.6.1: `pmatch()` at its default `duplicates.ok = FALSE` claims a formal once, returning one hit and one `NA`, so the survivor stayed `confi`, R's exact match consumed `config`, and `confi` fell into `...`; plain `do.call(f, list(conf = "x", confi = "y"))` raises `formal argument "config" matched by multiple actual arguments`. Fixed with `duplicates.ok = TRUE` plus a collision check that aborts pre-flight naming the formal and every argument matching it — batch-wide, like the `config` check beside it, asserted by `boundary_tools()` being `character(0)`. F5 fixed in the same lines: `targets` now stops at `...`, so a post-dots formal is not abbreviated where R would leave the name in `...`.
- 2026-08-09: T12 adds no NEWS entry: `match_formals()` is this branch's own helper, so the swallowed argument was never released behavior — the round-2 finding is a defect in unmerged work, not a change to what users have.
- 2026-08-09: T11-T12 verify slot MEASURED on R 4.6.1 / macOS 15: `devtools::document()` writes no diff, `devtools::test()` 1137 tests 0 failures 0 errors 6 skips, `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes in 43.8s. Against the 1009 the round-1 line records for the same command, this round adds 128.
- 2026-08-09: status → review.

## Decisions

## Review

_Evidence gathered 2026-08-09 on R 4.6.1 / macOS 15 (Darwin 25.6.0), at branch
`m19-guards-name-the-file`, PR #20._

- **AC1 — verified.** Domain re-derived by its stated input:
  `grep -n "stopifnot\|cli_abort" R/use_*.R` (44 hits, against 74 at T2 — the
  drop is the rewritten guards). Every surviving `stopifnot()` in
  `R/use_*.R` sits in a `*_dir()` pre-flight block — `of_extract_dir`
  `use_openface.R:155-158`, `aw_prep_audio_dir` `use_whisper.R:249-252`,
  `aw_transcribe_dir` `:544-549`, `os_prep_audio_dir` `use_opensmile.R:292-295`,
  `os_extract_dir` `:510-516` — which is exactly the set T2 recorded as not
  batch-reachable, with its reason; the three other `stopifnot` hits are inside
  comments. The 38 batch-reachable guards now route through `abort_file()`
  across eight functions (`os_check_audio`, `os_prep_audio`, `os_extract_wav`,
  `of_extract`, `aw_check_audio`, `aw_prep_audio`, `aw_transcribe`,
  `aw_transcribe_wav`) plus `os_fix_csv`'s own abort. 38 guard-message tests
  ran, 0 failures, each asserting `basename(infile)` and a defect substring.

- **AC2 — verified.** `os_extract_dir()` driven over a one-file directory with
  `wavdir=` and `aggdir=`, ffprobe reporting a non-conforming input and ffmpeg
  exiting 0 while writing nothing — the measured shape of the DESIGN
  2026-08-08 defect. The returned row reads `status = "failed"` and its `error`
  reads `Could not process 'clip.mp4'.` / `ffmpeg wrote no output at
  '<wavdir>/clip.wav'.` The test asserts the derived wav path in full and the
  ffmpeg attribution; the bare deparse it replaces appears nowhere.

- **AC3 — verified.** Three tests, all green. (1) `os_check_config("egemaps/v99/nope")`
  aborts with a message carrying `egemaps/v99/nope` and pointing at
  `os_list_configs()`. (2) `os_extract_dir(..., config = "egemaps/v99/nope")`
  signals an error rather than returning a table, and `boundary_tools()` is
  `character(0)` — no call at all, which is the assertion the criterion asks
  for rather than the weaker "no openSMILE call": the per-file path reaches
  ffprobe long before openSMILE, so an absence-of-openSMILE assertion would
  have passed over a batch that had already probed every input. (3) With the
  config directory emptied and NO `config` argument supplied, the same abort
  fires naming `misc/emo_large` — the default read from `formals(os_extract)`,
  which is the path `...` never carries.

- **AC4 — verified.** `os_fix_csv()` on a path nothing wrote aborts with
  `Could not tidy the openSMILE output at '<path>'.` / `openSMILE wrote no
  output there.`; the test asserts the full path and the attribution
  separately. The attribution's honesty was re-checked against both call sites
  rather than assumed: `os_extract_wav()` calls it only at
  `R/use_opensmile.R:444` and `:447`, each on the `aggfile`/`lldfile` it passed
  openSMILE as `-csvoutput` / `-lldcsvoutput` in the command built at `:433-439`
  and run at `:441`, so the file's absence is openSMILE having written nothing
  there. There is no third caller (`grep -n "os_fix_csv(" R/` returns the
  definition and those two).

- **AC5 — verified.** `devtools::document()` run at review leaves the tree
  clean (no `man/`, `NAMESPACE` or `DESCRIPTION` diff). `devtools::test()`:
  308 tests, 0 failures, 0 errors, 6 skips. `devtools::check()`: **Status OK —
  0 errors, 0 warnings, 0 notes** in 48.4s; the pre-existing `spelling` NOTE
  the criterion allows for did not appear at all, `spelling.R` passing as a
  test instead.

- **Consistency gate — passed.** `cairn_validate.py` exit 0: 16 PASS, 8 OK
  advisories, no FAIL. No `DESIGN.md` IP/GP principle changed (the edit is a
  Known-issues correction), so `cairn_impact` is a clean no-op. Toolchain
  slot (`r-package`): `document()` no diff; generated files regenerated, not
  hand-edited; README.Rmd/README.md untouched by the branch and last written by
  the same commit; no pkgdown site in the repo, so that check no-ops and no
  reference-index row is owed (nothing new is exported); NEWS.md carries two
  entries for this milestone's user-visible changes, with no milestone number
  in them; no new top-level files; `check()` clean as recorded under AC5.

### Independent review, round 1 (2026-08-09)

Three fresh-context lenses. **Blame-history [S]** — no findings: M18's
abort-vs-skip split, M17's `source` idea and `run_checked()`'s
value-interpolation rule are all preserved, and the deleted tests' coverage
migrated. **Prior-PR-comments [S]** — no regressions; its probe
(`gh api repos/jmgirard/openac/pulls/comments?per_page=1`) returned `[]`, so
the archived `## Review` sections were the whole surface, and M17 finding B and
M14's A3 whitespace trap are both honored by this diff. **Diff-bug [O]** — 18
candidate findings, scored by a fresh [S] scorer that reproduced findings
1, 2, 3, 4, 5, 6, 12 and 14 in R rather than accepting the claims.

**Actioned (>= 80), 7 of 18:**

- **F1 (93)** `R/use_opensmile.R:213`, `R/use_whisper.R:122` — `!is_integerish(stream, n = 1) || stream < 0` yields `NA` for a typed `NA_integer_`, because `is_integerish(NA_integer_, n = 1)` is TRUE; `if (NA)` then raises `missing value where TRUE/FALSE needed`, naming no file.
- **F2 (92)** `R/use_opensmile.R:131`, `:206`, `R/use_whisper.R:115`, `R/use_openface.R:75` — `if (!file.exists(infile))` raises `the condition has length > 1` / `argument is of length zero` for a non-scalar `infile`, bypassing `abort_file()`; the `stopifnot()` it replaced tolerated both.
- **F3 (90)** `R/use_opensmile.R:410`, `R/use_whisper.R:422` — `if (!os_check_audio(infile))` raises the same `NA` condition error when the SECOND ffprobe query returns an `NA` field; the `anyNA(streams)` early return covers only the stream count.
- **F5 (92)** `R/use_opensmile.R:529` — the pre-flight reads `extra_args$config` exactly, but `do.call(os_extract, ...)` PARTIAL-matches, so `conf = "nope/missing"` bypasses the check entirely and the batch probes every file.
- **F6 (90)** `R/use_opensmile.R:531` — with openSMILE unresolved, `os_list_configs()` calls `dirname(NULL)` and the whole batch dies on `a character vector argument expected` before `os_check_config()`'s own abort; pre-M19 this was a per-file failure the batch survived (GP6).
- **F8 (85)** `cairn/DESIGN.md:279-291` and `NEWS.md` assert three things the branch does not do: that every per-file guard routes through `abort_file()` (`os_fix_csv()` does not), that config is resolved once rather than per file (`os_extract_wav()` still calls `os_check_config()` per file, N -> N+1), and that a bad config costs no ffprobe rounds (false under F5).
- **F14 (82)** `R/utils.R:229-232` — messages now carry width-wrapped hard line breaks and a bullet glyph into the `error` column, and `dir_walk()`'s warning names the file twice.

**Logged, below the 80 bar (11 of 18), surfaced not dropped:**
F4 (68) the non-conforming-audio message tells the user to run `os_prep_audio()`
after openac just ran it · F7 (62) `os_fix_csv()` hand-rolls its abort instead
of using the shared helper · F9 (75) `abort_file()`'s `class` argument is never
passed and neither condition class is asserted anywhere · F10 (70)
`aw_transcribe_wav()`'s new `source` has no test covering `source != infile` ·
F11 (52) `basename(character(0))` degrades to `Could not process .` · **F12 (78)
`os_extract_dir(..., config = NULL)` pre-flights the DEFAULT and then fails per
file with a message naming no file — below the bar but demonstrating AC3's
clause, so it returns with the actioned set** · F13 (38) `eval(formals(...))`
uses the wrong environment if the default ever stops being a literal · F15 (35)
`get(flag)` resolves lexically · F16 (65) the basename assertions pass on a
full-path substring, so they do not discriminate · F17 (42) the deleted tests'
implicit "no boundary call before validation" proof is weaker now · F18 (45)
the `.env = parent.frame()` idiom is load-bearing and undocumented.

**Disposition: return to `in-progress` under the M130 return floor.** Six
findings demonstrate an acceptance criterion failing as written — F1, F2, F3
against AC1's "signals an error whose message names the file it stopped on and
the defect", inside the domain AC1's own grep names; F5, F6, F12 against AC3's
"`os_extract_dir()` validates `config` before `dir_walk()` runs" and "signals an
error whose message contains the config value it could not resolve". AC1 and AC3
are unticked: their evidence was real but tested a narrower domain than the
criteria claim, and both are now falsified. AC2, AC4 and AC5 stand verified and
are untouched by these findings. First defect return for this milestone.

### Round 2 (2026-08-09)

_Evidence gathered 2026-08-09 on R 4.6.1 / macOS 15 (Darwin 25.6.0), at branch
`m19-guards-name-the-file` (8f73e5f), PR #20. `main` at 212a234, unmoved since
the branch was cut, so the branch needed no merge._

- **AC1 — verified.** Domain re-derived by the criterion's own stated input:
  `grep -n "stopifnot\|cli_abort" R/use_*.R` returns 43 hits (44 at round 1;
  the drop is `ffp_count_streams()`'s hand-written scalar guard moving into the
  shared `check_file_arg()`). Every `stopifnot()` among them outside a comment
  sits in a `*_dir()` pre-flight block — `os_prep_audio_dir` `:321-324`,
  `os_extract_dir` `:541-547`, `of_extract_dir` `use_openface.R:156-159`,
  `aw_prep_audio_dir` `use_whisper.R:259-262`, `aw_transcribe_dir` `:557-562` —
  which is exactly the set T2 recorded as not batch-reachable, with its reason;
  the remaining `cli_abort` hits are `os_check_config` (AC3's), `os_fix_csv`
  (AC4's), and the three readers Scope excludes. The 38 batch-reachable guards
  route through `abort_file()` (32 call sites in `R/use_*.R`, of_extract's
  eight flags driven from one). Round 1 falsified this criterion not on the
  messages but on three guards never reached: 63 test blocks ran in
  `test-guard-messages.R`, 206 expectations, 0 failures — 38 of them the
  per-guard basename-and-defect cases the criterion asks for, plus 2 typed-`NA`
  `stream` cases (F1), 9 non-scalar `infile` cases (F2) and 5 blank-ffprobe-field
  cases (F3), each asserting the openac condition by class or by named substring
  rather than that some error occurred.
- **AC2 — verified.** `os_extract_dir()` driven fresh over a one-file directory
  with `wavdir=` and `aggdir=`, ffprobe reporting a non-conforming input and
  ffmpeg exiting 0 while writing nothing. The row reads `status = "failed"` and
  its `error` reads, verbatim and on one line, `Could not process 'clip.mp4':
  ffmpeg wrote no output at '<wavdir>/clip.wav'.` The test asserts the derived
  wav path in full and the ffmpeg attribution; the bare deparse it replaces
  appears nowhere.
- **AC3 — verified.** Six tests, all green, and the criterion re-read against
  the wider domain that falsified it. (1) `os_check_config("egemaps/v99/nope")`
  aborts with a message carrying `egemaps/v99/nope` and pointing at
  `os_list_configs()`. (2) an unresolvable `config =` makes `os_extract_dir()`
  signal rather than return, with `boundary_tools()` `character(0)` — no call at
  all. (3) the same holds with no `config` supplied, the default read from
  `formals(os_extract)`. New this round, closing round 1's falsifications:
  (4) `conf =` — the abbreviation `do.call()` would have partial-matched past
  the check — is now resolved by `match_formals()` and pre-flighted, tools still
  `character(0)`; (5) an explicit `config = NULL` is checked as the supplied
  value it is rather than treated as absent; (6) an unresolved openSMILE names
  openSMILE instead of dying on `dirname(NULL)`'s "a character vector argument
  expected", asserted as an absence as well as a presence.
- **AC4 — verified.** `os_fix_csv()` on a path nothing wrote aborts with
  `Could not tidy the openSMILE output at '<path>'.` / `openSMILE wrote no
  output there.`; the test asserts the full path and the attribution separately,
  and the guard-case table asserts the basename. The attribution's honesty was
  re-checked at both call sites this round, not carried over: `grep -n
  "os_fix_csv(" R/` returns the definition and exactly two callers, both inside
  `os_extract_wav()` on the file it just handed openSMILE as `-csvoutput` /
  `-lldcsvoutput`.
- **AC5 — verified.** `devtools::document()` run at review leaves the tree clean
  (no `man/`, `NAMESPACE` or `DESCRIPTION` diff). `devtools::test()`: 1009
  tests, 0 failures, 0 errors, 6 skips. `devtools::check()`: **Status OK —
  0 errors, 0 warnings, 0 notes** in 44.2s; the pre-existing `spelling` NOTE the
  criterion allows for did not appear, `spelling.R` passing as a test instead.

- **Consistency gate — passed.** `cairn_validate.py` exit 0: 16 PASS, 8 OK
  advisories, no FAIL. No `DESIGN.md` IP/GP principle changed (the edit is a
  Known-issues correction), so `cairn_impact` is a clean no-op. Toolchain slot
  (`r-package`): `document()` no diff; the one generated file in the diff
  (`man/os_extract_dir.Rd`) regenerates rather than being hand-edited;
  README.Rmd and README.md are untouched by the branch and last written by the
  same commit (2fba1c6); no pkgdown site, so that check and the reference-index
  row no-op; NEWS.md carries this milestone's user-visible changes with no
  milestone number in them; no new top-level files; `check()` clean as recorded
  under AC5.

### Independent review, round 2 (2026-08-09)

Three fresh-context lenses. **Blame-history [S]** — no findings: the
`abort_file()` rewrite has no pre-M19 contract to undo (the helper is this
branch's own), M18's skip/failure split and M14's GP6 resilience survive the
`dir_walk()` edit, and `ffp_count_streams()`'s guard moved into
`check_file_arg()` with the same message and position. **Prior-PR-comments [S]**
— no regressions; its probe (`gh api repos/jmgirard/openac/pulls/comments?per_page=1`)
returned `[]` again, so the archived `## Review` sections were the whole
surface, and M14's A3 wrap trap and A9 scalar-guard finding are both honored.
**Diff-bug [O]** — 20 candidate findings, scored by a fresh [S] scorer that
reproduced the 80+ set in R. Four scored at or above the bar; all four were then
re-reproduced by the reviewing session itself before being recorded.

**Actioned (>= 80), 4 of 20:**

- **F1 (95)** `R/use_opensmile.R:604-613` — `os_fix_csv()` hand-rolls a
  two-element `cli::cli_abort()`, so round 1's F14 fix never reached it, and it
  is batch-reachable through `os_extract_wav()` (`:444`, `:447`). MEASURED: a
  row's `error` reads `Could not tidy the openSMILE output at
  '<...>/clip.csv'.\n<glyph> openSMILE wrote no output there.` — the newline and
  glyph F14 removed, in the one guard it did not cover. This falsifies three
  claims the branch itself added: NEWS.md's "one line, so it stays readable in
  the `error` column", DESIGN's "an abort of the same shape", and the round-2
  AC4 evidence line above.
- **F2 (90)** `tests/testthat/test-guard-messages.R:646-673`, `:41-47` — nothing
  could have caught F1: the one-line test drives only the `abort_file()` path,
  and every guard-case assertion runs through `collapsed_guard()`, whose
  `gsub("\\s+", " ", ...)` deletes the newline under test.
- **F3 (85)** `R/utils.R:139-143` — `abort_file()`'s eager `format_inline()`
  bakes ANSI colour codes into the `error` column whenever colours are on.
  MEASURED with `cli.num_colors = 256`: `"Could not process \033[34mclip.wav\033[39m:
  No file exists at \033[34m/nope/clip.wav\033[39m."` Same class as the glyph
  and the wrap, and the F14 test asserts on neither.
- **F6 (92)** `R/utils.R:202-204` — `match_formals()` uses `pmatch()`, which is
  greedy, so two distinct prefixes of one formal resolve to one rename and one
  survivor. MEASURED against live R: `f(1, conf = "x", confi = "y")` raises
  `formal argument "config" matched by multiple actual arguments`; after
  `match_formals()` the `do.call()` succeeds with `config = "x"` and `confi`
  swallowed into `...`. The helper turns an error R would raise into silence,
  and a user's second argument is ignored rather than rejected. The scorer's
  stated consequence — N per-file duplicate-argument errors — did NOT reproduce
  and is recorded here as overstated; the measured consequence is the silent
  swallow.

**Logged, below the 80 bar (16 of 20), surfaced not dropped:**
F5 (78) `match_formals()` also partial-matches formals sitting AFTER `...`,
which R does not — latent, since `os_extract()`'s `config` precedes `...` ·
F12 (78) DESIGN's corrected sentence names two exceptions to `abort_file()` and
`os_check_config()` inside `os_extract_wav()` is a third · F14-rate (70)
`!isTRUE(dat[[2]] == "44100")` now recommends a sampling rate for a value it
could not read · F10 (68) the basename assertion does not discriminate for the
nine cases whose defect clause already carries the full path (round 1's F16) ·
F9 (65) `abort_file()`'s `class` argument is never passed and
`openac_file_guard` is asserted nowhere (round 1's F9) · F11 (60)
`aw_transcribe_wav()`'s `source != infile` branch is untested (round 1's F10) ·
F7 (55) `os_check_audio()`'s new `length(dat) < 3` branch says "no audio stream"
for a truncated probe, its query having no `-select_streams a` — the scorer
found its serious half pre-existing on `main` · F8 (55) `check_file_arg(source)`
is unreachable in both callers · F13 (55) the stream-index message pairs a
0-based index with a count · F15 (55) `os_list_configs()`'s `@return` does not
record the new abort, and `require_program()` now runs twice per
`os_check_config()` · F16 (50) `abort_file()` accepts a vector `message`, which
would reintroduce F14 · F4 (45) the batch now dies when openSMILE is
unresolved — the scorer found this intentional, logged at the plan gate and
tested this round, with only the `@return` gap unaddressed · F17 (40)
`eval(formals(...))` frame (round 1's F13) · F18 (35) `get(flag)` resolves
lexically (round 1's F15) · F20 (35) the whitespace collapse runs pre-
interpolation — the scorer showed `format_inline()`'s own `strip_newline`
default already backs the invariant · F19 (30) `e$defect` partial-matches.

**Disposition: return to `in-progress` under the M130 return floor.** No
acceptance criterion fails: all five were verified above and none of the four
actioned findings falsifies one as written — F1's one-line property was actioned
"beyond the criteria" at T10 and appears in no criterion's text, and F6's
duplicate-prefix call resolves a config that IS resolvable, so no clause of AC3
is touched. The return is under the floor's other limb: **F1 (95) and F6 (92)
are both scored at or above 90 on defects in what the package does for its
users** — a batch row's `error` column carrying a newline and a glyph, and a
supplied argument being silently ignored. Second defect return for this
milestone; the thrash rule's third-return threshold is not yet reached, and its
same-criterion trigger does not fire, since round 1's failures were AC1 and AC3
and this round's findings rest on neither.

### Round 3 (2026-08-09)

_Evidence gathered 2026-08-09 on R 4.6.1 / macOS 15 (Darwin 25.6.0), at branch
`m19-guards-name-the-file` (6902e5e), PR #20. `main` at 212a234, unmoved since
the branch was cut, so the branch needed no merge._

- **AC1 — verified.** Domain re-derived by the criterion's own stated input:
  `grep -n "stopifnot\|cli_abort" R/use_*.R` returns 43 hits (43 at round 2 —
  `os_fix_csv()`'s `cli_abort` left, and the comment recording why arrived).
  Every `stopifnot()` among them outside a comment sits in a `*_dir()`
  pre-flight block — `os_prep_audio_dir` `use_opensmile.R:321-324`,
  `os_extract_dir` `:541-547`, `of_extract_dir` `use_openface.R:156-159`,
  `aw_prep_audio_dir` `use_whisper.R:259-262`, `aw_transcribe_dir` `:557-562` —
  exactly the set T2 recorded as not batch-reachable, with its reason; four
  further `stopifnot` hits are inside comments. The surviving `cli_abort` hits
  are `os_check_config` `use_opensmile.R:99`, `:107` (AC3's, and a `*_dir()`
  pre-flight) and the three readers Scope excludes (`os_read` `:666-675`,
  `of_read` `use_openface.R:203-212`, `aw_read_data` `use_whisper.R:644-673`).
  `os_fix_csv()` is no longer among them: it routes through `abort_file()`,
  which now has 33 call sites in `R/use_*.R` (32 at round 2, +1 for that move),
  one comment mention excluded from the count. The domain is 30 case-table
  guards plus `of_extract()`'s 8 flags = the 38 T2 enumerated.
  `test-guard-messages.R` ran 96 test blocks, 334 expectations, 0 failures,
  0 errors, 0 skips.
- **AC2 — verified.** `os_extract_dir()` driven fresh over a one-file directory
  with `wavdir=` and `aggdir=`, ffprobe reporting a non-conforming input and
  ffmpeg exiting 0 while writing nothing. The row reads `status = "failed"` and
  its `error`, verbatim, is `Could not process 'clip.mp4': ffmpeg wrote no
  output at '<wavdir>/clip.wav'.` — one line, no glyph, and (new this round)
  `identical()` to its own `cli::ansi_strip()`. The full derived wav path is
  present: a first comparison read FALSE and was chased rather than recorded,
  and the cause was the harness, not the message — `withr::local_tempdir()`
  returns `T//Rtmp…` while the guard names the `fs::path_abs()` form; against
  the absolutized path the match is TRUE.
- **AC3 — verified.** Six clauses, each driven fresh rather than read off the
  suite. (1) `os_check_config("egemaps/v99/nope")` aborts with `Can't find the
  openSMILE config "egemaps/v99/nope".` (2) an unresolvable `config =` makes
  `os_extract_dir()` signal rather than return, `boundary_tools()` length 0 —
  no call at all. (3) the same with no `config` supplied, the default read from
  `formals(os_extract)` (the emptied-config-directory test in the suite).
  (4) `conf =` is resolved by `match_formals()` and pre-flighted, tools length
  0. (5) an explicit `config = NULL` is checked as the supplied value it is:
  `` `config` must be a single string, not NULL. `` (6) an unresolved openSMILE
  names openSMILE rather than dying on `dirname(NULL)`. New alongside them,
  from T12: `conf =` and `confi =` together abort with `` `config` is matched by
  more than one argument: `conf` and `confi`. ``, tools length 0.
- **AC4 — verified.** `os_fix_csv()` on a path nothing wrote aborts with
  `Could not process 'agg.csv': openSMILE wrote no output at '<path>'.`,
  condition class `openac_file_guard`. The wording changed this round — the
  guard moved onto `abort_file()` — and the criterion's two parts are both
  measured on the new text: the full path is present, and the openSMILE
  attribution is present. The attribution's honesty was re-checked at both call
  sites rather than carried over: `grep -n "os_fix_csv(" R/` returns the
  definition and exactly two callers, both inside `os_extract_wav()` on the
  file it just handed openSMILE as `-csvoutput` / `-lldcsvoutput`.
- **AC5 — verified.** `devtools::document()` leaves the tree clean (no `man/`,
  `NAMESPACE` or `DESCRIPTION` diff). `devtools::test()`: 1137 tests, 0
  failures, 0 errors, 6 skips. `devtools::check()`: **Status OK — 0 errors,
  0 warnings, 0 notes** in 1m10s; the pre-existing `spelling` NOTE the criterion
  allows for did not appear, `spelling.R` passing as a test instead.

- **Consistency gate — passed.** `cairn_validate.py` exit 0: 16 PASS, no FAIL,
  one advisory — `sizing (split tripwires)`, M19 at 12 tasks against a
  >10 tripwire. Recorded rather than waved past: the count is 10 tasks of
  planned work plus 2 added by defect returns, so it measures the returns, not
  a mis-cut plan, and the tripwire is advisory by design. No `DESIGN.md` IP/GP
  principle line changed (the edit is a Known-issues correction), so
  `cairn_impact` is a clean no-op. Toolchain slot (`r-package`): `document()`
  no diff; the one generated file in the diff (`man/os_extract_dir.Rd`)
  regenerates rather than being hand-edited; README.Rmd and README.md are
  untouched by the branch and last written by the same commit (2fba1c6); no
  pkgdown site, so that check and the reference-index row no-op, and NAMESPACE
  is not in the diff so nothing new is exported; NEWS.md carries this
  milestone's user-visible changes with no milestone number in them; no new
  top-level files; `check()` clean as recorded under AC5.
