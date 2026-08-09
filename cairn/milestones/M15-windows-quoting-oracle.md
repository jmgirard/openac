# M15: What Windows actually does to a path the shell can eat

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP7
- **Branch/PR:** m15-windows-quoting-oracle / #15

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

- [x] AC1 `test-real-tools.R` gains one `test_that()` per entry of a hostile-name
      table naming at least a space, `%TEMP%`, `^`, `&`, `!` and an apostrophe.
      Each writes a `.wav` at that name through real ffmpeg and asserts the
      name openac passed is present in `list.files()` of its directory, reporting
      what is there when it is not. Gates are per-test (`skip_on_cran`,
      `check_ffmpeg`/`check_ffprobe`); no gate is conditional on the OS, so no
      entry can pass by skipping on Windows.
- [x] AC2 The Review section records, per table entry, what AC1 measured on the
      Windows host — intact, or what stood in its place — beside that host's
      `R.version.string` and `Sys.info()[["release"]]` as printed there, dated.
      Transcribed from the run, never from expectation.
- [x] AC3 Every entry AC2 records as not intact arrives intact after this
      milestone's change, shown by AC1's tests passing on that host; an entry
      that survives the composed `cmd`+`cmd2` form is instead recorded here and
      in DESIGN's known issues as a dated limitation. If AC2 records every entry
      intact, `run_tool()`'s quoting behavior is unchanged and its comment
      (`R/run_tool.R:22-24`) cites the dated measurement rather than `shQuote`'s
      documented default.
- [x] AC4 An internal takes the quoting rule as an argument and `run_tool()`
      calls it with the platform's; a test asserts its literal output for every
      AC1 table entry under the Windows rule, so the rule reddens the suite on
      macOS and Linux too. Written after AC2 and citing it.
- [x] AC5 The `LESSONS.md` line dated 2026-08-08 (M13) on Windows `%` is either
      confirmed by AC2's measurement or corrected in place and marked, whichever
      AC2 supports; DESIGN's "Calling the CLIs" paragraph states the quoting
      rule the code ends with.
- [x] AC6 `devtools::test()` passes, `devtools::check()` reports 0 errors, 0
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
- [x] T2 **[Windows host]** With ffmpeg and ffprobe present, run the full suite
      including `test-real-tools.R`; transcribe the per-entry result and the two
      version strings into the milestone.
- [x] T3 **[Windows host]** Change `R/run_tool.R:58`'s quoting for whatever T2
      measured mangled — plain `cmd`, then the composed `cmd`+`cmd2` — re-running
      T2's tests there until each entry is intact or recorded as a limitation.
      Nothing measured mangled: rewrite the `:22-24` comment instead.
- [x] T4 Extract the type-taking internal and add its cross-platform test with
      literal expected strings.
- [x] T5 Confirm or correct the `LESSONS.md` `%` line; update DESIGN's "Calling
      the CLIs" paragraph, and its known issues if T3 recorded a limitation.
- [x] T6 `devtools::document()`, `devtools::test()`, `devtools::check()`; open the
      PR and confirm all five CI jobs green.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: implement started on m15-windows-quoting-oracle.
- 2026-08-08: T1 done — `hostile_names()` in helper-openac.R holds 8 entries (space, `$`, `%TEMP%`, `^`, `&`, `!`, apostrophe, backtick), each also carrying a space; test-real-tools.R loops one bare `test_that()` per entry. All 8 pass on macOS; full suite 623 pass, 0 fail, 2 expected skips, contract gate clean.
- 2026-08-08: T1 mutation check — replacing `shQuote(arg)` at R/run_tool.R:58 with literal `paste0('"', arg, '"')` reddens exactly the `dollar` and `backtick` entries (plus M13's own oracle at :55), the two characters sh expands inside double quotes; the other six are inert under sh and carry their weight only on the Windows run. So the table discriminates here rather than passing vacuously. Mutation reverted, `git status` clean.
- 2026-08-08: T1 commit first swept the mutation run's debris (`tests/testthat/_problems/`, `testthat-problems.rds`) in via `git add -A` — the M13 lesson's exact shape, third occurrence. Removed and both paths added to `.gitignore` so testthat's failure artifacts cannot be staged again.
- 2026-08-08: T2 done on the maintainer's Windows 11 host (build 26100, R 4.6.1, ffmpeg and ffprobe resolved through openac's own config rather than `PATH`) — all eight hostile names round-tripped intact, none skipped. Full record in Review. This falsifies the second plan-gate reason logged below: the `cmd`+`cmd2` composition was never needed, because there is no interpreter to escape for.
- 2026-08-08: T3 took AC3's third branch — nothing measured mangled, so `run_tool()`'s quoting behavior is unchanged and the `:22-24` comment was rewritten to cite the measurement (and to say why `cmd2` is wrong here, not merely unnecessary) instead of `shQuote`'s documented default.
- 2026-08-08: T5 widened beyond its wording — the falsified `%` claim was not only in `LESSONS.md` and DESIGN but in the user-facing `@param arg` roxygen of all four passthroughs (`use_ffmpeg.R`, `use_ffprobe.R`, `use_openface.R`, `use_opensmile.R`), which told users a `%TEMP%` path "can still be expanded". The Goal says "the claims this repo makes about it", so all four were corrected in the same pass.
- 2026-08-08: the Windows run also reddened `OpenFace really extracts features that of_read() can read`. Checked against `main` before attributing it: it fails there identically, so it is not M15's. Diagnosis and disposition in Review.
- 2026-08-08: T6 done — PR #15 opened, all five R-CMD-check jobs green. Local `check()` is 0 warnings / 0 notes with one error, the pre-existing OpenFace real-tools failure.
- 2026-08-08: plan gate chose two milestones (M15 quoting, M16 installers) over one Windows branch, because a combined goal needs an "and" and the two cross the acceptance-criteria and task tripwires together; falsified by the installer run proving to depend on M15's quoting change.
- 2026-08-08: plan gate chose composing `shQuote("cmd")` with `"cmd2"` as the fallback fix over hand-rolling a quoter, because `?shQuote` documents that composition as the intended Windows form and a hand-rolled quoter has no local test loop on a platform this session cannot run; falsified by the composed form measuring mangled on the host.
- 2026-08-08: plan chose a real-ffmpeg round-trip oracle over asserting `shQuote()` output alone, because the open question is whether `system2()` puts `cmd.exe` in the loop at all — `?system2` says it "allows redirection of output without needing to invoke a shell on Windows" — which no assertion over quoting output can answer; falsified by the round trip proving unable to distinguish an expanded name from an absent tool.
- 2026-08-08: review — status was still `in-progress` when review opened; the Windows session finished T6 without the completion transition, so review made it here and logged it rather than treating it as an override.
- 2026-08-08: review fan-out — [O] diff-bug 16 findings, [S] blame-history 6, [S] prior-PR-comments 0; scorer actioned F1 (85) and F5 (80), both fixed on the branch. Five below-bar findings fixed as well because each was a false statement this branch wrote (F2, F15, F11, F10, F3/F4); F9 rejected on checking; the rest logged. Full triage in Review.
- 2026-08-08: review — maintainer directed repo-wide LF line endings at the gate, closing F8/B4 (78) properly rather than by reverting: five remaining CRLF files normalized and `.gitattributes` added so a Windows edit cannot reintroduce the mix.
- 2026-08-08: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 satisfiable by skipping everywhere, AC2's per-entry record unobtainable from one aborting `test_that`, AC3 missing the not-repairable branch and contradicting AC4, AC4 presupposing a single base type, AC5 unsatisfiable if the lesson proves true — plus a flag that both AC2s could be satisfied by prose alone. All were fixed in the wording above before the gate; none became a gate question.

## Decisions

## Review

### AC2 — the Windows measurement, 2026-08-08

Host, as printed by the run:

- `R.version.string` — `R version 4.6.1 (2026-06-24 ucrt)`
- `Sys.info()[["release"]]` — `10 x64` (`Sys.info()[["version"]]` reports
  `build 26100`; `COMSPEC` is `C:\WINDOWS\system32\cmd.exe`, so a command
  interpreter exists on this host and is simply not in the loop)

`test-real-tools.R` run via `testthat::test_local(filter = "real-tools")` with
real ffmpeg and ffprobe present (`check_ffmpeg()` and `check_ffprobe()` both
`TRUE`, so no entry skipped — transcribed from the per-test result frame, where
every row below reads `skipped = FALSE`, `failed = 0`, `passed = 3`):

| Entry | Name written | What landed in `list.files()` | Verdict |
|---|---|---|---|
| space | `a space.wav` | `a space.wav` | intact |
| dollar | `a $dollar.wav` | `a $dollar.wav` | intact |
| percent | `a %TEMP% token.wav` | `a %TEMP% token.wav` | intact |
| caret | `a ^caret.wav` | `a ^caret.wav` | intact |
| ampersand | `a &ampersand.wav` | `a &ampersand.wav` | intact |
| bang | `a !bang.wav` | `a !bang.wav` | intact |
| apostrophe | `a Jeff's clip.wav` | `a Jeff's clip.wav` | intact |
| backtick | `` a `backtick`.wav `` | `` a `backtick`.wav `` | intact |

All eight round-tripped: ffmpeg wrote the name openac passed, and ffprobe read
that same file back and reported 1 audio / 0 video streams for it.

`shQuote()` on the same host, for the record AC4's literals rest on — the
default and `type = "cmd"` are identical for every entry, and `cmd2` is the only
style that rewrites anything:

| Entry | `shQuote(x)` = `type = "cmd"` | `type = "cmd2"` |
|---|---|---|
| space | `"a space.wav"` | `a space.wav` |
| dollar | `"a $dollar.wav"` | `a $dollar.wav` |
| percent | `"a %TEMP% token.wav"` | `a ^%TEMP^% token.wav` |
| caret | `"a ^caret.wav"` | `a ^^caret.wav` |
| ampersand | `"a &ampersand.wav"` | `a ^&ampersand.wav` |
| bang | `"a !bang.wav"` | `a ^!bang.wav` |
| apostrophe | `"a Jeff's clip.wav"` | `a Jeff's clip.wav` |
| backtick | `` "a `backtick`.wav" `` | `` a `backtick`.wav `` |

**What this settles.** M13's open question was whether `system2()` puts
`cmd.exe` between openac and the tool on Windows. It does not. `cmd` quoting
leaves `%`, `^`, `&` and `!` bare, and they arrived bare and unexpanded — so
there is nothing to escape against, and the `cmd2` style M15 planned as its
fallback fix would have escaped for an interpreter that never sees the string.
The measurement takes AC3's third branch: `run_tool()`'s quoting behavior is
unchanged.

### AC6

`devtools::document()`, `devtools::test()` and `devtools::check()` run on the
Windows host: **0 warnings, 0 notes**, and one error — the OpenFace real-tools
test below, which fails identically on `main` here.

All five `R-CMD-check` jobs green on #15 (run 31286088382, 2026-08-08):
`macos-latest (release)`, `ubuntu-latest (devel)`, `ubuntu-latest (oldrel-1)`,
`ubuntu-latest (release)`, `windows-latest (release)`. CI installs no external
binaries, so `test-real-tools.R` skipped on every runner — the green
`windows-latest` job evidences the suite and never AC2's measurement, exactly
as AC6 says.

### Review verification (macOS host, 2026-08-08)

Fresh evidence gathered at review. Every AC box was already ticked by the
implementing session, so each is re-verified here and its tick stands on the
line below rather than on that earlier pass.

- **AC1** — `devtools::test()` runs all eight hostile-name tests and passes
  them (646 pass, 0 fail, 2 skips: OpenFace absent, whisper model absent).
  Gates are per-test; grepping `test-real-tools.R` for `skip_on_os`,
  `OS.type` and `sysname` returns nothing, so no gate is OS-conditional and no
  entry can pass by skipping. The T1 work-log line records the mutation that
  reddens it.
- **AC2** — the Windows round trip cannot be re-derived from macOS, so what is
  verified here is the record's internal consistency: its eight rows match
  `hostile_names()` exactly, and its `type = "cmd"` column reproduces
  character-for-character on this host, that column being the platform-
  independent half of the record. The round-trip column rests on the
  implementing session's transcription from the host, as AC2 provides for.
- **AC3** — behavior-unchanged measured rather than read:
  `identical(quote_tokens(x, quote_type()), shQuote(x))` is `TRUE` for a vector
  carrying both `%TEMP%` and an apostrophe, and the length-1 form returns its
  input untouched. `R/run_tool.R`'s comment cites the dated measurement, and at
  review gained the boundary of what that measurement does not cover.
- **AC4** — `quote_type()` and `quote_tokens()` exist, `run_tool()` calls them,
  and their tests pass on macOS, which is the criterion's whole point. The
  `quote_type()` oracle was strengthened at review (F11).
- **AC5** — `LESSONS.md`'s M13 line is corrected in place and marked;
  DESIGN's "Calling the CLIs" paragraph states the rule the code ends with.
  Both were further corrected at review (F15, F3, F4).
- **AC6** — `devtools::test()` clean; `devtools::check()` **Status: OK** on
  macOS — 0 errors, 0 warnings, 0 notes — both before and after the review
  fixes; all five `R-CMD-check` jobs pass on #15 for the final review commit
  (run 31289464034, after the line-ending normalization). The Windows
  host's own check reported one error, the pre-existing OpenFace failure that
  fails on `main` there too; AC6 names no host, it is met on this one, and that
  Windows error now has a candidate row rather than no owner.

### Consistency gate

`cairn_validate` — 16 PASS, 8 advisory OK, exit 0. `devtools::document()`
produces no diff. README unchanged by this branch; no pkgdown site. NEWS.md
carries the user-visible correction. No principle changed, so `cairn_impact`
was not run. `.gitattributes` is the one new top-level file and `check()` is
clean with it present.

### Independent review — three lenses, then a scorer

[O] diff-bug returned 16 findings, [S] blame-history 6 (one a defect, five
clean verdicts), [S] prior-PR-comments zero — that lens confirmed M13's B1, B2,
B5 and B9 are not regressed, and its GitHub inline-comment probe returned empty
so no thread walk was paid for.

**Actioned (scored ≥80):**

- **F1 (85) — NEWS.md still ships the falsified `%TEMP%` claim to users.**
  T5 swept LESSONS, DESIGN and the four `@param arg` blocks; NEWS was not among
  them, leaving the package asserting both the corrected and the falsified
  claim, with the falsified one on its most user-facing surface. FIXED — the
  note now records that the earlier warning was wrong and what was measured
  instead.
- **F5 (80) — the corrected paragraph still calls the length-1 path "passed
  through to the shell".** All four `@param arg` blocks and DESIGN retained the
  model M15 had just falsified, in the same paragraph as the correction. FIXED
  in all five places.

**Below the bar, fixed anyway** — each is a statement this branch wrote that is
false as written, which is not a matter of taste:

- F2 (72) — the roxygen claimed `%TEMP%`, `&`, `^`, `!` "were measured reaching
  the tool intact" for openface and opensmile; the measurement went through
  ffmpeg and ffprobe only, and OpenFace is the one tool that produced no
  positive evidence on that host. Rewritten to state what openac does, which is
  true of all four, and the provenance moved to `run_tool.R` and DESIGN.
- F15 (72) — LESSONS said M13's "first sentence held" when only its first
  clause was measured; the cmd.exe-expands-`%VAR%` clause, which is the one the
  falsified conclusion rode on, never was. Re-marked.
- F11 (75) — the `quote_type()` test's second expectation compared
  `shQuote(c("-i", "a b.mp4"))` against itself under the named type, which a
  regression to `"csh"` would satisfy. The reviewer's suggested repair (a
  `!`-bearing name) was MEASURED not to work — no single table entry separates
  `sh` from `csh` — but the table taken as a vector does, because the
  apostrophe entry pushes both styles off the single-quote branch and they then
  diverge on `$`. Re-pointed at the whole table.
- F10 (65) — `quote_type()`'s comment described `quote_tokens()`'s callers and
  contradicted itself. Rewritten.
- F3 (58) and F4 (42) — the mechanism claim was stated with no scope, and
  "eight names round-tripped" read as eight independent confirmations when
  `cmd.exe` would have left seven of them alone anyway (`^`, `&` and a backtick
  are inert inside double quotes; `!` needs delayed expansion). One entry,
  `a %TEMP% token.wav`, carries the conclusion. Both `run_tool.R` and DESIGN now
  say so and name the axes the measurement does not cover.

**Logged, not actioned:**

- F8 / B4 (78, reached independently by two lenses) — partial CRLF→LF
  conversion of two files inflated the diff by ~330 lines and re-pointed their
  blame. Below the action bar, but the maintainer directed the fix at the
  review gate: every tracked file is now LF and `.gitattributes` pins it, so
  the mixed state that caused it is gone rather than merely reverted.
- F7 (65) — the OpenFace-writes-no-CSV defect had no home outside this file.
  Now a ROADMAP candidate row.
- F9 (30) — asserted `quote_tokens()`'s stated rationale is false because
  `run_tool()` is directly testable. Checked: `quote_tokens()` is exercised
  without `local_fake_tools()` while `run_tool()` is not, so the rationale
  holds. Rejected.
- F6 (62) — AC6 ticked against a Windows check reporting one error. AC6 names
  no host and is met on macOS with 0/0/0; the Windows error is pre-existing and
  now owned by a candidate row. No amendment convened.
- F13 (62) — `run_tool()`'s wiring to the two internals reddens only on
  `windows-latest`. Real, and that job runs on every PR. Left.
- F12 (55), F14 (30), F16 (12) — a redundant-but-harmless assertion, a
  set-vs-list edge case with no trigger, and line lengths. Left.

### Out of scope, observed on the same host

`OpenFace really extracts features that of_read() can read` fails here, and
**fails identically on `main`** (checked by stashing this branch and re-running
the file) — not a regression from M15. OpenFace itself is installed and works:
it opens the video, tracks it to completion, and writes `faces_of_details.txt`
beside the requested `faces.csv`, but writes no `.csv` at all, apparently
because the synthetic `testsrc` pattern contains no face. The test's own comment
says the macOS run gets a header-only CSV. That is a real cross-platform
difference in OpenFace's output contract, not openac's quoting; recorded here
and left for its own hotfix or candidate rather than absorbed into M15.
