# M19: A guard that names no file

- **Status:** in-progress
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

- [ ] AC1 Every guard T2's work-log enumeration classifies as batch-reachable
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
- [ ] AC3 `os_check_config()` signals an error whose message contains the
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
- [x] T7 `devtools::document()`, `devtools::test()`, `devtools::check()`.

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
