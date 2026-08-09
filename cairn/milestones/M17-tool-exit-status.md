# M17: A tool that exited non-zero is a failed file

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP6, GP9
- **Branch/PR:** —

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

- [ ] AC1 The low-level passthroughs still return `system2()`'s value rather
      than erroring on a non-zero exit. A test calls all eight exported
      bindings — `ffmpeg`/`ffm`, `ffprobe`/`ffp`, `openface`/`of`,
      `opensmile`/`os`, which D-010 records as separate bindings — under
      `fake_nonzero_exit()` (`tests/testthat/helper-openac.R:860`) and asserts
      each returns a value carrying a `status` attribute.
- [ ] AC2 Each of the four per-file call sites T3 wires — `os_prep_audio()`,
      `aw_prep_audio()`, `os_extract_wav()` and `of_extract()` — raises an
      error on a non-zero tool exit whose message, with whitespace collapsed
      as `collect_warnings()` documents (`helper-openac.R:896-905`), contains
      the input file's basename, the program name and the exit status. One
      test per site, four in total.
- [ ] AC3 `os_prep_audio_dir()` records a file whose ffmpeg conversion exits
      non-zero as `success = FALSE` with that message in the `error` column;
      the `os_prep_audio_dir` half of the KNOWN GAP test
      (`tests/testthat/test-batch-dirs.R:334`) is rewritten to assert it.
- [ ] AC4 The check reads the exit status from the `status` attribute and
      never from R's warning text, which is translated (LESSONS, M14): a test
      drives it under `fake_nonzero_exit()`'s non-English default message and
      asserts the error still fires.
- [ ] AC5 `devtools::document()` shows no drift, `devtools::test()` passes,
      and `devtools::check()` reports 0 errors, 0 warnings and no NOTE other
      than the pre-existing `spelling` NOTE (PR #9's recorded baseline).

## Coverage

- AC1 → T4
- AC2 → T1, T2, T3
- AC3 → T3, T5
- AC4 → T1, T2
- AC5 → T6

## Tasks

- [ ] T1 Add the internal exit-status check (`R/run_tool.R` or `R/utils.R`),
      reading `attr(x, "status")` and suppressing R's own status warning by
      position — mirror the mechanism and comments at `R/use_ffprobe.R:120-160`.
- [ ] T2 Test-first: one failing test per tool asserting the error names the
      file, the program and the status, using `fake_nonzero_exit()`.
- [ ] T3 Call the check from the per-file wrapper sites — `os_prep_audio()`
      (`R/use_opensmile.R:201`), `aw_prep_audio()`, `os_extract_wav()` and
      `of_extract()`; confirm the site list against
      `grep -n "ffmpeg(\|opensmile(\|openface(" R/use_*.R` less the
      passthrough definitions.
- [ ] T4 Add the AC1 test pinning the exported passthroughs' unchanged return.
- [ ] T5 Rewrite the `os_prep_audio_dir` half of the KNOWN GAP test; update
      the NEWS.md paragraph that names it (`NEWS.md:13-16`).
- [ ] T6 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned five findings on this file — AC4 unsatisfiable (its grep matches three explanatory comments today), AC2 indefinite where ffmpeg has two per-file wrappers, AC1 over-claiming "unchanged" and omitting the four exported aliases, AC5 comparing against an unnamed baseline; all five fixed before the gate, none deferred.
- 2026-08-08: plan gate chose the exit-status check in the per-file callers over one check inside `run_tool()` because `run_tool()` has no file context and the message must name the file, and because `ffp_count_streams()` would need an opt-out to keep its contractual NA return (M14); falsified by a call site needing the check where the caller cannot know the file.
- 2026-08-08: plan gate chose three milestones over one because the combined scope is ~15 criteria and ~20 tasks, well past the split tripwires; falsified by the three proving inseparable in implementation.

## Decisions

## Review
