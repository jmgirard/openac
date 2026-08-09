# M17: A tool that exited non-zero is a failed file

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** `m17-tool-exit-status` · https://github.com/jmgirard/openac/pull/18

## Goal

Make a non-zero exit from ffmpeg, openSMILE or OpenFace an error naming the
file, the tool and the status, so a batch records that file as a failed row
instead of a success.

## Scope

**In:** M14 made `ffp_count_streams()` read the `status` attribute
`system2(stdout = TRUE, stderr = TRUE)` sets (`R/use_ffprobe.R:146`); it is
still the only place in the package that reads one, so an ffmpeg, openSMILE or
OpenFace failure is invisible to its caller (`R/run_tool.R:99` returns
`system2()`'s value verbatim). A shared internal check, called by the per-file
wrapper functions that know which file is being processed, aborts naming the
file, the program and the exit status. R's own exit-status warning is
suppressed by position, never by text — it is translated (LESSONS, M14). Wired
into the per-file `ffmpeg()`, `opensmile()` and `openface()` call sites.
NEWS entry.

**Out:** The four exported low-level passthroughs keep returning `system2()`'s
value unchanged, `status` attribute included — the check lives in the callers
that know the file, not in `run_tool()`. `ffp_count_streams()`'s own status
handling is contractual since M14 and is untouched. Deliberate skips recorded
as successes → M18. Guards whose message names no file → M19. Output-path
collisions → the standing ROADMAP candidate, behind M18.

## Acceptance criteria

- [x] AC1 The low-level passthroughs still return `system2()`'s value rather
      than erroring on a non-zero exit. A test calls all eight exported
      bindings — `ffmpeg`/`ffm`, `ffprobe`/`ffp`, `openface`/`of`,
      `opensmile`/`os`, which D-010 records as separate bindings — under
      `fake_nonzero_exit()` (`tests/testthat/helper-openac.R:860`) and asserts
      each returns a value carrying a `status` attribute.
- [x] AC2 Each of the four per-file call sites T3 wires — `os_prep_audio()`,
      `aw_prep_audio()`, `os_extract_wav()` and `of_extract()` — raises an
      error on a non-zero tool exit whose message, with whitespace collapsed
      as `collect_warnings()` documents (`helper-openac.R:896-905`), contains
      the input file's basename, the program name and the exit status. One
      test per site, four in total.
- [x] AC3 `os_prep_audio_dir()` records a file whose ffmpeg conversion exits
      non-zero as `success = FALSE` with that message in the `error` column;
      the `os_prep_audio_dir` half of the KNOWN GAP test
      (`tests/testthat/test-batch-dirs.R:334`) is rewritten to assert it.
- [x] AC4 The check reads the exit status from the `status` attribute and
      never from R's warning text, which is translated (LESSONS, M14): a test
      drives it under `fake_nonzero_exit()`'s non-English default message and
      asserts the error still fires.
- [x] AC5 `devtools::document()` shows no drift, `devtools::test()` passes,
      and `devtools::check()` reports 0 errors, 0 warnings and no NOTE other
      than the pre-existing `spelling` NOTE (PR #9's recorded baseline).

## Coverage

- AC1 → T4
- AC2 → T1, T2, T3
- AC3 → T3, T5
- AC4 → T1, T2
- AC5 → T6

## Tasks

- [x] T1 Add the internal exit-status check (`R/run_tool.R` or `R/utils.R`),
      reading `attr(x, "status")` and suppressing R's own status warning by
      position — mirror the mechanism and comments at `R/use_ffprobe.R:120-160`.
- [x] T2 Test-first: one failing test per tool asserting the error names the
      file, the program and the status, using `fake_nonzero_exit()`.
- [x] T3 Call the check from the per-file wrapper sites — `os_prep_audio()`
      (`R/use_opensmile.R:201`), `aw_prep_audio()`, `os_extract_wav()` and
      `of_extract()`; confirm the site list against
      `grep -n "ffmpeg(\|opensmile(\|openface(" R/use_*.R` less the
      passthrough definitions.
- [x] T4 Add the AC1 test pinning the exported passthroughs' unchanged return.
- [x] T5 Rewrite the `os_prep_audio_dir` half of the KNOWN GAP test; update
      the NEWS.md paragraph that names it (`NEWS.md:13-16`).
- [x] T5b (added at implementation) A GP7 layer-2 test in
      `test-real-tools.R`: a real failing ffmpeg really sets the status the
      wrapper reads — the one assertion the mocked boundary cannot make.
- [x] T6 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned five findings on this file — AC4 unsatisfiable (its grep matches three explanatory comments today), AC2 indefinite where ffmpeg has two per-file wrappers, AC1 over-claiming "unchanged" and omitting the four exported aliases, AC5 comparing against an unnamed baseline; all five fixed before the gate, none deferred.
- 2026-08-08: plan gate chose the exit-status check in the per-file callers over one check inside `run_tool()` because `run_tool()` has no file context and the message must name the file, and because `ffp_count_streams()` would need an opt-out to keep its contractual NA return (M14); falsified by a call site needing the check where the caller cannot know the file.
- 2026-08-08: implementation gate confirmed both plan recommendations — the check covers all three tools including OpenFace (unverifiable locally, mocked-boundary only), and the error carries the tool's own last output lines.
- 2026-08-08: MEASURED on this host (R 4.6.1, macOS 15, ffmpeg 8.0) before writing the check: a SUCCESSFUL ffmpeg run sets NO `status` attribute (NULL, not 0), a failing one sets 254 and R warns `... had status 254`. Both facts are load-bearing — `status != 0` would abort every successful call.
- 2026-08-08: T1–T5 done. `run_checked()` added to R/run_tool.R and wired into the four per-file sites; 17 tests in the new test-tool-exit-status.R, red before the change (16 failures) and green after; suite 753 pass / 0 fail / 6 pre-existing skips.
- 2026-08-08: D-010's command-contract gate reddened on the new function, exactly as designed — `run_checked` entered the computed `system2` closure with no command test. Satisfied with a real command test asserting it forwards its tokens quoted, not with a deferral entry.
- 2026-08-08: added T5b, a GP7 layer-2 real-ffmpeg test, after noting the mocked suite is structurally blind to whether a real failing tool sets `status` at all — the shape of blindness M16 found in the mocked installer suite. Mutation-verified: neutering the check reds it (2 failures), restoring it passes 44.
- 2026-08-08: T6 done, status → review. `devtools::check()` 0 errors / 0 warnings / 0 notes; `devtools::document()` no drift; suite 753 pass / 0 fail; real-tools layer 44 pass with ffmpeg, ffprobe and openSMILE present, OpenFace and audio.whisper skipped as absent.
- 2026-08-09: review round 1 RETURNED (defect return 1). Finding A (93): `run_checked()` nests `withCallingHandlers(tryCatch(...))` where M14's sibling nests `tryCatch(withCallingHandlers(...))`, so the error handler's warning replay is re-captured by the still-active handler and the `set_program()` hint is lost from all four wrappers — MEASURED, 0 warnings surfaced against the passthrough's 1. Also actioned: D (85) two status assertions that cannot fail, B (84) `os_extract()` naming a tempfile NEWS says is the user's file, F (82) no test for the error path. 15 findings logged sub-threshold.
- 2026-08-09: correcting the 2026-08-08 T1-T5 entry above, which is history and so is superseded rather than edited (D-045): `test-tool-exit-status.R` had 12 `test_that()` blocks at that commit, not "17 tests" — the figure was hand-written rather than derived, the defect the derived-figure rule deletes. It now has 14.
- 2026-08-09: round-1 fixes. A: nesting transposed to `tryCatch(withCallingHandlers(...))` and M14's "not ceremony" rationale restored; mutation-verified, the inverted form reds the new test. F: a test drives the abort path through all four wrappers, asserting the `set_program()` hint survives. B: `os_extract_wav()` gained `source`, so `os_extract()` names the user's file, not the temp wav; mutation-verified. D: the four site assertions now pin the whole phrase `<tool> exited with status <n>` rather than a digit.
- 2026-08-09: sub-threshold notes actioned alongside — NEWS narrowed (openSMILE already failed the row via `os_fix_csv()`, with an unreadable message), DESIGN narrowed (the two unchecked second-ffprobe calls named), and a candidate row opened for those two calls.
- 2026-08-08: plan gate chose three milestones over one because the combined scope is ~15 criteria and ~20 tasks, well past the split tripwires; falsified by the three proving inseparable in implementation.

## Decisions

## Review

**AC1 — verified 2026-08-08.** `test-tool-exit-status.R` "every passthrough and
alias returns a non-zero exit rather than erroring": 9 passing expectations —
all eight bindings (`ffmpeg`/`ffm`, `ffprobe`/`ffp`, `openface`/`of`,
`opensmile`/`os`) return a value whose `status` attribute is `3L` under
`fake_nonzero_exit(status = 3L)`, plus the boundary-call count. No binding
errors.

**AC2 — UNTICKED 2026-08-09**, finding D: two of the four site tests assert
`"1"` and `"11"` against a message carrying a random-hex tempfile basename, so
they pass on noise and do not demonstrate the exit status reaching the message.
The openSMILE and OpenFace sites are unverified until those assertions pin the
phrase. Round-1 evidence, now insufficient: Four tests, one per wired site, 4 passing
expectations each: `os_prep_audio()`, `aw_prep_audio()`, `os_extract_wav()` and
`of_extract()` each raise `openac_tool_failed` whose whitespace-collapsed
message contains the input basename, the program name and the exit status
(254, 254, 1, 11 respectively).

**AC3 — verified 2026-08-08.** `test-batch-dirs.R` "os_prep_audio_dir() records
a failed conversion as a failed row": 3 passing expectations. Confirmed against
the REAL ffmpeg outside the mocked boundary — a text file named `clip.mp4`
yields `success = FALSE`, no wav written, and `error` reading
`Could not process 'clip.mp4'. ffmpeg exited with status 183. ffmpeg said:
… Error opening input: Invalid data found when processing input …`.

**AC4 — verified 2026-08-08.** "a non-English status warning still produces the
error" passes: the check fires under `fake_nonzero_exit()`'s French default
message, so it cannot be keyed on R's English text. The companion test "R's own
status warning is suppressed, and nothing else is" (3 expectations) shows the
suppression is positional — a preceding unrelated warning survives, the trailing
status warning does not.

**AC5 — verified 2026-08-08.** `devtools::check()` 0 errors / 0 warnings /
0 notes (43.3s, openac 0.1.0.9000). `devtools::test()` 753 pass / 0 fail /
6 skips, all pre-existing (4 opt-in installer probes, OpenFace absent,
audio.whisper absent). `devtools::document()` produces no drift.

**Consistency gate — 2026-08-08.** `cairn_validate` exit 0, all checks passed.
`cairn_impact --changed`: no changed principles (M17 works under GP6/GP9, it
changes neither). Profile `consistency-gate` slot: `document()` no-diff
verified; no generated file hand-edited.

**Round 1 — RETURNED 2026-08-09 (defect return 1).** Three fresh-context lenses
(diff-bug [O], blame-history [S], prior-review [S]) reported 19 distinct
findings; a fresh [S] scorer with the diff and the plan scored them. Four
scored ≥80 and are actioned; 15 scored below 80 and are logged.

Actioned (≥80), all four to be fixed on the branch:

- **A (93) — `run_checked()` inverts M14's handler nesting, and the
  `set_program()` hint is lost.** `R/run_tool.R:135-147` is
  `withCallingHandlers(tryCatch(...), warning=)` where the sibling
  `ffp_count_streams()` (`R/use_ffprobe.R:119-131`) is
  `tryCatch(withCallingHandlers(...), error=)`. The error handler's replay
  therefore runs while the calling handler is still established, so every
  released warning is re-captured and muffled. MEASURED with the tool absent
  from `PATH`: `os_prep_audio()` surfaces 0 warnings where `ffmpeg()` surfaces
  1. All four wired wrappers lose the hint; before this branch they did not.
  This is M14 fix-delta F1 reintroduced, in the one function told to mirror it.
- **D (85) — two AC2 status assertions cannot fail.**
  `test-tool-exit-status.R:114` and `:126` assert `"1"` and `"11"` against a
  message carrying a random-hex tempfile basename, so they pass on noise and
  cannot distinguish status 1 from no status at all. AC2's wording is
  technically met, which is how they passed the criteria audit.
- **B (84) — `os_extract()` names a temp path that exists nowhere.** It passes
  `infile = wavfile` to `os_extract_wav()` (`R/use_opensmile.R:302-321`), and
  `wavfile` is `tempfile()` on the default path `os_extract_dir()` uses, so the
  abort reads `Could not process 'file122de76bef6fe.wav'.` NEWS advertises
  `os_extract()` as naming the file you can re-run.
- **F (82) — no test drives `run_checked()`'s error path at all.** Every test
  drives a returned status; nothing drives `run_tool()` erroring. That is the
  hole A shipped through, and the sibling has a pinning test for exactly this
  behavior (`test-commands-probe.R:258-273`).

Logged, sub-threshold (15): S (66) `basename()` vs the full path
`ffp_count_streams()` argues for under `recursive = TRUE`; Q (63) work-log says
17 tests where the file has 12 `test_that()` blocks; I (63) DESIGN's new
"exit status is no longer among the gaps" overstates while the second ffprobe
query stays unchecked; M (62) no layer-2 openSMILE test though it is installed
locally; N (62) `expect_false(file.exists(outfile))` pins ffmpeg's behavior not
openac's; H (76) cli renders the tool's lines as an English list with an
inserted "and"; E (74) `test-batch-dirs.R:351` asserts on the un-collapsed
message (latent); C (68) NEWS overclaims openSMILE was previously a silent
success; G (55) the positional last-warning drop is unconditional; R (45) the
mechanism is duplicated rather than shared; L (45) OpenFace is mocked-only by
recorded plan decision; O (42) `infile` unvalidated; J (30) and P (25)
pre-existing; K (5) stale — the DESIGN correction is committed in `b94a29b`.

**Round 2 — fix delta `5d01530`, reviewed [O] fresh-context, scored [S].**
The reviewer confirmed A and B genuinely fixed and B's test genuinely
discriminating, both mutation-verified independently. Eleven findings on the
delta; three scored ≥80, all records-or-tests rather than shipped behavior, so
none tripped the return floor and all three were fixed in place:

- **1 (90) — the new error-path test claimed four wrappers and reached two.**
  MEASURED: with nothing resolvable, `aw_prep_audio()` aborts inside
  `ffp_count_streams()` and `os_extract_wav()` inside `os_check_audio()`, so
  neither reached `run_checked()`. Rewritten so each case starves only its own
  tool; `os_extract_wav()` is now excluded WITH ITS REASON — `os_check_config()`
  resolves the config directory relative to the openSMILE binary, so
  `run_tool()`'s not-found abort is unreachable from it. Three is the whole
  domain, not a sample of four, and the test title says three. Re-verified:
  the inverted nesting now reds all three cases.
- **4 (90) — the round-1 DESIGN correction introduced a new false claim.**
  "Exit status is now read at the four per-file wrapper sites and nowhere else"
  — `ffp_count_streams()` reads it too, as M17's own Scope says. The same
  defect class round 1 corrected in that very paragraph. Now: "read at exactly
  two places", both named.
- **5 (83) — a DESIGN claim this branch falsified and the delta walked past.**
  `os_extract_dir()` was described as recording the bare deparse
  `file.exists(infile) is not TRUE`. MEASURED 2026-08-09 with real ffmpeg: it
  now reads `Could not process 'clip.mp4'. ffmpeg exited with status 183. …`.
  Corrected in place and marked.

Two sub-threshold findings were fixed anyway, being one-line hardenings of
tests authored this session: 3 (73) the four status assertions were substring
matches with no right boundary, so appending a digit to the status left them
green — MEASURED; they now carry the trailing period, and the same mutation
reds all four. 2 (66) the loop reported every case at one source line; each
case now carries `info`.

Logged, not actioned: 6 (50) `-instname` still labels output rows with the temp
wav — pre-existing, same class as B, now visible beside `source`; 8 (50) an
abort leaks the temp wav because `unlink()` follows the call rather than sitting
in `on.exit()`; 7 (52) the B test drives an explicit `wavfile`, so the literal
`tempfile()` default is not exercised on the failure path (the code path is
identical — `source = infile` in both branches); 10 (57) a direct
`os_extract(x)` with both outputs NULL was a silent success and is now an abort,
which NEWS does not mention; 11 (40) and 9 (32) minor.

**Gate re-run on the delta.** `devtools::test()` 762 pass / 0 fail / 6
pre-existing skips. `devtools::check()` 0 errors / 0 warnings / 0 notes.
`devtools::document()` no drift. `cairn_validate` exit 0.

**Correction made at review.** `DESIGN.md`'s Known-issues GP6 entry asserted two
things this milestone falsified — that `os_prep_audio_dir()` records a bad file
as a success, and that "`run_tool()` inspects no tool's exit status … 
`ffp_count_streams()` is the only place in the package that reads one".
Corrected in place and marked (current knowledge, D-045). This is the defect
class M14 was returned for twice; it was caught here rather than by a reviewer.
