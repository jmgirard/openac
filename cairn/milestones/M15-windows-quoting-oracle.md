# M15: What Windows actually does to a path the shell can eat

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP7
- **Branch/PR:** m15-windows-quoting-oracle

## Goal

Settle by measurement on a real Windows host what reaches an external tool when
a filename carries a `cmd.exe` metacharacter, and make openac's Windows quoting —
and the claims this repo makes about it — match the answer.

## Scope

**In:** a hostile-name oracle in `tests/testthat/test-real-tools.R`, one
`test_that()` per name, asserting through real ffmpeg/ffprobe that the file
openac named is the file on disk; the first run of that file on Windows;
whatever change to `R/run_tool.R:58`'s quoting the run shows is needed, up to
composing `shQuote(type = "cmd")` with `type = "cmd2"` as `?shQuote` describes;
a type-taking internal so the Windows rule is asserted from any host; the
`LESSONS.md` `%` line and DESIGN's "Calling the CLIs" paragraph brought to what
was measured.

**Out:** the Windows installers and the OneDrive model links → M16. A
hand-rolled quoter of openac's own, if the composed form still fails → recorded
as a dated limitation here, and a candidate row for the fix. Re-validating the
harness's simulated Windows rules → dropped at the 2026-08-08 gate: M09
measured them on real Windows runners and `LESSONS.md` records the result. The
command-display surface → existing candidate.

## Acceptance criteria

- [ ] AC1 `test-real-tools.R` gains one `test_that()` per entry of a hostile-name
      table naming at least a space, `%TEMP%`, `^`, `&`, `!` and an apostrophe.
      Each writes a `.wav` at that name through real ffmpeg and asserts the
      name openac passed is present in `list.files()` of its directory, reporting
      what is there when it is not. Gates are per-test (`skip_on_cran`,
      `check_ffmpeg`/`check_ffprobe`); no gate is conditional on the OS, so no
      entry can pass by skipping on Windows.
- [ ] AC2 The Review section records, per table entry, what AC1 measured on the
      Windows host — intact, or what stood in its place — beside that host's
      `R.version.string` and `Sys.info()[["release"]]` as printed there, dated.
      Transcribed from the run, never from expectation.
- [ ] AC3 Every entry AC2 records as not intact arrives intact after this
      milestone's change, shown by AC1's tests passing on that host; an entry
      that survives the composed `cmd`+`cmd2` form is instead recorded here and
      in DESIGN's known issues as a dated limitation. If AC2 records every entry
      intact, `run_tool()`'s quoting behavior is unchanged and its comment
      (`R/run_tool.R:22-24`) cites the dated measurement rather than `shQuote`'s
      documented default.
- [ ] AC4 An internal takes the quoting rule as an argument and `run_tool()`
      calls it with the platform's; a test asserts its literal output for every
      AC1 table entry under the Windows rule, so the rule reddens the suite on
      macOS and Linux too. Written after AC2 and citing it.
- [ ] AC5 The `LESSONS.md` line dated 2026-08-08 (M13) on Windows `%` is either
      confirmed by AC2's measurement or corrected in place and marked, whichever
      AC2 supports; DESIGN's "Calling the CLIs" paragraph states the quoting
      rule the code ends with.
- [ ] AC6 `devtools::test()` passes, `devtools::check()` reports 0 errors, 0
      warnings and no note absent from a same-day check of the default branch,
      and all five `R-CMD-check` jobs are green. CI installs no binaries, so a
      green `windows-latest` job evidences AC6 alone and never AC2.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1 Write the hostile-name table and its per-entry tests; confirm on macOS
      that every entry passes (M13 fixed the unix side) and that the file still
      clears `test-zzz-command-contract.R`'s top-level-skip gate.
- [ ] T2 **[Windows host]** With ffmpeg and ffprobe present, run the full suite
      including `test-real-tools.R`; transcribe the per-entry result and the two
      version strings into the milestone.
- [ ] T3 **[Windows host]** Change `R/run_tool.R:58`'s quoting for whatever T2
      measured mangled — plain `cmd`, then the composed `cmd`+`cmd2` — re-running
      T2's tests there until each entry is intact or recorded as a limitation.
      Nothing measured mangled: rewrite the `:22-24` comment instead.
- [ ] T4 Extract the type-taking internal and add its cross-platform test with
      literal expected strings.
- [ ] T5 Confirm or correct the `LESSONS.md` `%` line; update DESIGN's "Calling
      the CLIs" paragraph, and its known issues if T3 recorded a limitation.
- [ ] T6 `devtools::document()`, `devtools::test()`, `devtools::check()`; open the
      PR and confirm all five CI jobs green.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: implement started on m15-windows-quoting-oracle.
- 2026-08-08: T1 done — `hostile_names()` in helper-openac.R holds 8 entries (space, `$`, `%TEMP%`, `^`, `&`, `!`, apostrophe, backtick), each also carrying a space; test-real-tools.R loops one bare `test_that()` per entry. All 8 pass on macOS; full suite 623 pass, 0 fail, 2 expected skips, contract gate clean.
- 2026-08-08: T1 mutation check — replacing `shQuote(arg)` at R/run_tool.R:58 with literal `paste0('"', arg, '"')` reddens exactly the `dollar` and `backtick` entries (plus M13's own oracle at :55), the two characters sh expands inside double quotes; the other six are inert under sh and carry their weight only on the Windows run. So the table discriminates here rather than passing vacuously. Mutation reverted, `git status` clean.
- 2026-08-08: plan gate chose two milestones (M15 quoting, M16 installers) over one Windows branch, because a combined goal needs an "and" and the two cross the acceptance-criteria and task tripwires together; falsified by the installer run proving to depend on M15's quoting change.
- 2026-08-08: plan gate chose composing `shQuote("cmd")` with `"cmd2"` as the fallback fix over hand-rolling a quoter, because `?shQuote` documents that composition as the intended Windows form and a hand-rolled quoter has no local test loop on a platform this session cannot run; falsified by the composed form measuring mangled on the host.
- 2026-08-08: plan chose a real-ffmpeg round-trip oracle over asserting `shQuote()` output alone, because the open question is whether `system2()` puts `cmd.exe` in the loop at all — `?system2` says it "allows redirection of output without needing to invoke a shell on Windows" — which no assertion over quoting output can answer; falsified by the round trip proving unable to distinguish an expanded name from an absent tool.
- 2026-08-08: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 satisfiable by skipping everywhere, AC2's per-entry record unobtainable from one aborting `test_that`, AC3 missing the not-repairable branch and contradicting AC4, AC4 presupposing a single base type, AC5 unsatisfiable if the lesson proves true — plus a flag that both AC2s could be satisfied by prose alone. All were fixed in the wording above before the gate; none became a gate question.

## Decisions

## Review
