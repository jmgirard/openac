# M13: Quote at the process boundary, not at the call site

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP5, GP7
- **Branch/PR:** `m13-boundary-quoting` · https://github.com/jmgirard/openac/pull/14

## Goal

Move shell quoting out of the seven hand-written command builders and into a
single boundary runner, so no call site can ship a broken command by forgetting
a quote.

## Scope

**In:** an internal boundary runner that resolves the program, applies
`shQuote()` per token with a platform-appropriate `type`, and calls `system2()`;
the four passthroughs (`ffmpeg`, `ffprobe`, `openface`, `opensmile`) routed
through it under the two-form contract chosen at the plan gate — a length-1
argument passes through unchanged as today, a longer character vector is one CLI
token per element; the seven internal assembly sites at `R/use_ffprobe.R:51`,
`R/use_opensmile.R:114,174,331`, `R/use_whisper.R:20,109,129` and
`R/use_openface.R:80` converted to token vectors; a harness-level check that
fails any boundary call still carrying an unquoted-whitespace element; the
command tests moved from collapsed strings to token boundaries.

**Out:** a user-facing way to display or return the constructed command (GP5's
remaining half) → ROADMAP candidate, cheap once this lands. `ffp_count_streams`'s
abort-on-bad-file behavior → M14. The openac/tidymedia name-collision note → a
direct docs commit to the default branch, not a milestone.

## Acceptance criteria

- [x] AC1 Each of `ffmpeg()`, `ffprobe()`, `openface()`, `opensmile()` treats a
      character vector of length > 1 as one CLI token per element and hands
      `system2()` one `shQuote()`-ed element per token, with the platform's
      `type`; a length-1 argument reaches `system2()` byte-identical to today.
      Evidence: per passthrough, one mocked-boundary test of each form asserting
      the recorded vector element by element via `boundary_argv()`.
- [x] AC2 Given a media path containing a space and a `$`, the element the
      boundary receives for that path is `shQuote()`'s rendering of the path and
      nothing else — asserted at the mocked boundary for `ffp_count_streams()`
      and `os_prep_audio()`, each test failing against the pre-milestone
      concatenated assembly. That the *shell* then delivers it as one argument is
      a separate claim, asserted in `test-real-tools.R` behind the existing
      real-binary gate by probing such a file and asserting stream counts rather
      than an error.
- [x] AC3 `local_fake_tools()`'s fake `system2()` aborts on any boundary call
      whose argument vector has length > 1 and contains an element carrying
      whitespace that `shQuote()` would have enclosed, and the full suite passes
      with that check armed. `test-helper-boundary.R`'s deliberate raw-argv
      fixtures opt out through an explicit `local_fake_tools()` argument, and
      that argument has no other caller (`grep -rn` over `tests/`). The claim is
      over the boundary calls the suite makes, not over every branch of every
      wrapper. Verified by mutation: restoring `ffp_count_streams()`'s
      concatenated assembly (`R/use_ffprobe.R:51-56`) turns the suite red.
- [x] AC4 No command assertion reads the collapsing accessor `boundary_args()`:
      `grep -n 'boundary_args' tests/testthat/test-commands-*.R` returns nothing.
      Its uses outside those three files are untouched.
- [x] AC5 `cairn/DESIGN.md`'s Architecture "Calling the CLIs" paragraph
      (`:164-174`) and the four passthroughs' roxygen `@param arg` and
      `@examples` describe both accepted forms; `devtools::document()` leaves no
      uncommitted diff.
- [x] AC6 `devtools::test()` passes and `devtools::check()` reports 0 errors, 0
      warnings, and no note absent from a check of the default branch run the
      same day.

## Coverage

- AC1 → T1, T2
- AC2 → T4, T5, T8
- AC3 → T3, T4, T5, T6
- AC4 → T7
- AC5 → T9
- AC6 → T9

## Tasks

- [x] T1 Add the internal boundary runner (new `R/run_tool.R`): resolve via
      `require_program()`, `shQuote()` each element when `length(arg) > 1`, else
      pass through; direct tests for the two forms and the platform `type`.
- [x] T2 Route the four passthroughs (`R/use_ffmpeg.R:23`, `R/use_ffprobe.R:23`,
      `R/use_openface.R:23`, `R/use_opensmile.R:23`) through it; the existing
      passthrough and alias tests in `test-commands-probe.R:13-40` must stay
      green unedited — that is the no-break check.
- [x] T3 Arm the unquoted-whitespace check in `local_fake_tools()`'s
      `fake_system2` (`helper-openac.R:596-638`), beside the existing
      absolute-path check, with the opt-out argument AC3 names; exempt
      `test-helper-boundary.R`'s raw fixtures.
- [x] T4 Convert `ffp_count_streams()` (`R/use_ffprobe.R:51`) and
      `os_check_audio()` (`R/use_opensmile.R:114`) to token vectors; add the
      space-and-`$` regression test that fails against the old assembly.
- [x] T5 Convert `os_prep_audio()` (`R/use_opensmile.R:174`), `aw_check_audio()`
      (`R/use_whisper.R:20`) and `aw_prep_audio()` including its `-af` filter
      branch (`R/use_whisper.R:109,129`).
- [x] T6 Convert `of_extract()` (`R/use_openface.R:80`) and `os_extract()`
      including its `aggfile`/`lldfile`/`-instname` branches
      (`R/use_opensmile.R:331`).
- [x] T7 Move the three `test-commands-*.R` files' assertions to
      `boundary_argv()`; drop `boundary_args()` from them.
- [x] T8 Add the real-`ffprobe` case in `test-real-tools.R` for AC2's shell half.
- [x] T9 Update DESIGN's Architecture paragraph and the roxygen `@param`/
      `@examples`; `devtools::document()`, `devtools::check()`, `inst/WORDLIST`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan gate chose the length-decides passthrough contract (D-017) over vector-only-with-a-dash-heuristic, vector-only-with-no-detection, and internal-helper-only, because it fixes every internal call site while breaking no existing call; falsified by a real call whose intent length cannot express — a single token carrying whitespace that must be quoted, or a multi-token vector that must reach the shell raw.
- 2026-08-08: plan gate chose to leave GP5's command-display surface out over shipping it with the quoting change, because M13 already spans 4 exported functions, 7 assemblers and 5 test files; falsified by the display surface turning out to require a different token representation than the one M13 lands.
- 2026-08-08: T1 done. MEASURED before writing anything: `system2()` does not quote `args` (a bare token vector reaches the tool split on spaces), and openac's current hand-quoted `paste0('-i "', path, '"')` loses a `$` in a path — `/tmp/a $b.mp4` was delivered as `/tmp/a .mp4`, a live bug M13 closes. `shQuote()` per token is correct on both axes and its default type is already platform-appropriate.
- 2026-08-08: T1's shell-oracle test runs the real `system2()` against a script echoing its own argv, mocking only discovery — the only test in the file that observes what the tool RECEIVES rather than what `system2()` was handed; mutation-verified (removing the quoting reds 5 assertions, the oracle among them).
- 2026-08-08: T2 done. The no-break check held as planned: `test-commands-probe.R:13-40`'s forwarding and alias tests pass unedited. One test outside that range did have to change — `passthroughs reject a non-string argument` asserted `ffprobe(c("-a","-b"))` errors, which D-017 makes valid; its multi-element case moved to a new positive assertion rather than being dropped.
- 2026-08-08: T3 done. The check is armed in `fake_system2` and tested positively — a guard never observed to fire cannot be told from one silently disarmed, and no real assembler trips it yet (they still pass length-1 glued strings, which are exempt), so nothing else would notice. Exactly one opt-out in the suite, at `test-helper-boundary.R:336`.
- 2026-08-08: T3's quote character is derived from `shQuote("x")` rather than written out, so the check stays strict per platform: a permissive both-quote-characters test would accept a hand-written `"..."` on unix, which is the form that still expands `$` — the measured bug itself, not a variant.
- 2026-08-08: T4 done. `ffp_count_streams()` and `os_check_audio()` now emit token vectors. AC2's regression uses a path carrying BOTH a space and a `$`: measured against the old form, a space alone still passed, so a space-only fixture would have pinned nothing — the `$` is the discriminating character.
- 2026-08-08: T4 mutation, both clauses. Restoring `ffp_count_streams()`'s concatenated assembly reds 5 assertions (via the command tests; the harness guard stays quiet there, because a length-1 glued string is the exempt legacy form). Breaking `run_tool()`'s quoting instead reds 7 and DOES trip the guard on real assemblers, naming the offending path. So the guard's live scope is a call site that reaches `system2()` without going through `run_tool()` — narrower than a reader of AC3 alone might assume, and worth saying plainly.
- 2026-08-08: T5 done. `os_prep_audio()`, `aw_check_audio()` and `aw_prep_audio()` converted; the `-af` chain is now two tokens (flag, whole chain as one value) and the `ifelse` that built it went away for a `character()` that vanishes inside `c()`.
- 2026-08-08: T5 uncovered a real coupling the plan missed: three tests used side-effecting fakes that recovered the output path from the glued args with `sub('^.*"([^"]+)"$', ...)`. Tokenisation silently defeated that regex, which then returned the shQuoted last element and made `file.create()` write a quote-named file — surfacing as a wrapper bug. Replaced by a `boundary_outfile()` helper that reads the last token and strips one layer of quoting.
- 2026-08-08: T5 mutation sharpens T4's note on the guard's scope. Gluing a flag to its value (`"-map 0:a:0"` as one token) reds 6 command tests but does NOT trip the harness guard, because `run_tool()` quotes that token before the fake sees it. So the guard cannot observe mis-tokenisation that goes through `run_tool()` at all; it observes only a call site reaching `system2()` without it. AC3 is satisfied as written, but its value should not be read wider than this.
- 2026-08-08: T6 done. `of_extract()` and `os_extract_wav()` converted. The `ifelse(flag, ' -x', '')` idiom does not survive tokenisation — `""` is a real empty argument once quoted, where the absence wanted is `character()` — so an `opt_arg()` helper in `R/run_tool.R` replaces it at all ten optional-flag sites.
- 2026-08-08: T6 replaced the substring assertions (`expect_match(args, ' -I "path"')`) with a `boundary_value(argv, flag)` accessor reading the token AFTER a flag. Strictly stronger than what it replaced: it fails on a wrapper that emits flag and value in the wrong order or glued into one token, which a substring match passed.
- 2026-08-08: T6 mutations both red — gluing `-multi_view 1` into one token reds 2, dropping the `-lldcsvoutput` pair reds 2. Also fixed a latent looseness in `test-batch-dirs.R`: its `(?<=csvoutput ")` lookbehind matched `-lldcsvoutput` by suffix as well; the two flags are now matched exactly.
- 2026-08-08: T7 done. Most of the conversion landed with T4-T6; T7 finished the two passthrough assertions that still read the collapsing accessor. They assert the legacy single-string form, where collapsing IS lossless — which is precisely why the collapsing accessor proves nothing there, so they moved too rather than being excepted. `grep -n 'boundary_args' tests/testthat/test-commands-*.R` now hits only the comment saying so.
- 2026-08-08: T8 done, and it RAN rather than skipped (ffmpeg and ffprobe are installed here). Real ffmpeg writes to a path carrying a space and a `$`, real ffprobe reads the same one back. Also converted `real_wav()`/`real_video()` to the token form — they were the last hand-quoted assemblers in the repo, in the one file that had never mocked anything.
- 2026-08-08: T8 mutation is the milestone's strongest evidence. Reverting `ffp_count_streams()` to the hand-quoted form makes real ffprobe exit status 1 and report 0 audio streams where 1 was expected: the shell expanded `$dollar` away, so the tool was asked for a file that does not exist. The failure identity is verified against the real tool, not inferred from a mock.
- 2026-08-08: T9 done. Four `@param arg` blocks and their examples document both forms, DESIGN's "Calling the CLIs" paragraph rewritten around `run_tool()`, two NEWS entries (the `$` fix, and the new vector form), `CLI` added to `inst/WORDLIST`. `devtools::document()` leaves no diff; `R CMD check` is 0 errors / 0 warnings / 0 notes.
- 2026-08-08: T9 found debris I committed myself at T5, and `R CMD check` is what caught it. The broken intermediate state at T5 had the old fakes running `sub('^.*"([^"]+)"$', ..., args)` against a token VECTOR; `sub()` and `file.create()` are both vectorized, so instead of erroring they created one zero-byte file per token in `tests/testthat/` — `'-y'`, `'0:a:0'`, `'-c:a'` and nine more. My `git add -A` then swept all twelve into bc7199f, which is exactly the stranger-sweeping the git model warns against. Removed in this commit; check now reports no non-portable file names, and a clean run creates none.
- 2026-08-08: a scripted edit had normalized `R/use_ffprobe.R` and `R/use_openface.R` from CRLF to LF as a side effect, burying ~37 real changed lines under ~400 of ending churn. Restored; the branch squash-merges, so the net diff is what lands and it is now 30 and 37 lines. Both files keep the endings they had on main — this milestone made no decision about line endings.
- 2026-08-08: M13 complete, status review. Suite 585 pass / 0 fail / 2 skip (the pre-existing OpenFace and whisper real-tool gates); `R CMD check` 0/0/0; `devtools::document()` no diff.
- 2026-08-08: review in progress, draft PR #14 open; CI green on all five jobs (macOS, Windows, Ubuntu release/devel/oldrel) — Windows is the only verification of the cmd-style quoting branch, which no local run reaches. Two of three review lenses reported zero findings; the diff-bug lens is still running.
- 2026-08-08: review found AC4 FAILS as written. Its procedure — `grep -n 'boundary_args' tests/testthat/test-commands-*.R` returns nothing — returns one line, a comment at `test-commands-probe.R:25` explaining why those assertions moved off the collapsing accessor. The substantive claim holds; the named procedure over-matches prose. Disposition pending the third lens, so one return covers everything.
- 2026-08-08: review RETURN 1 to in-progress, two causes. (a) AC4 fails inside its own named procedure's domain: the grep it specifies returns a comment. (b) Actioned finding B1 (scored 80): the harness guard's quote character is derived as `substr(shQuote("x"),1,1)` = `'`, but `shQuote(type="sh")` switches to DOUBLE quotes when the string contains an apostrophe — MEASURED `shQuote("Jeff's clip.mp4")` -> `"Jeff's clip.mp4"` — so a correct call on an apostrophe-bearing path aborts, blaming the call site. Reproduced end to end.
- 2026-08-08: three review lenses ran; blame-history and prior-review returned zero findings each, the diff lens returned 12. It verified all seven assemblers token-by-token equivalent to main — no dropped flag, changed value, or reorder — so the production conversion itself is clean.
- 2026-08-08: return 1 resolved. AC4 fixed by moving the explanatory comment to `helper-openac.R` beside `boundary_args()`'s definition; B1 fixed by accepting either quote character, which is weaker than the strict version and recorded as such in the Review section.
- 2026-08-08: writing B1's regression test surfaced a second instance of the same class, which no reviewer found: `shQuote()` chooses its quoting style for the whole VECTOR, not per element — MEASURED, `shQuote(c("-i", "Jeff's.mp4"))` double-quotes BOTH elements while `shQuote("-i")` alone single-quotes. So `boundary_value()`'s scalar `shQuote(flag)` comparison silently matched nothing whenever any element of that argv held an apostrophe. It now matches on unquoted values.
- 2026-08-08: second debris incident — a file named `first`, from a malformed heredoc, swept into cf650ad by `git add -A` and caught by `R CMD check`, not by me. Twice on one branch is a pattern: `git add -A` after any failed command needs `git status` read first.
- 2026-08-08: post-return verification: suite 598 pass / 0 fail / 2 skip; `R CMD check` 0/0/0; `cairn_validate` clean; AC3's named mutation re-confirmed red with the mutation verified present in the file before running.
- 2026-08-08: Windows CI FAILED on a test I wrote during the return. `test-helper-boundary.R:390` asserted the two argvs start with different characters — true only on unix, where sh-style quoting has two branches; cmd-style has one, so on Windows both start with `"` and the assertion is false. MEASURED locally via `shQuote(..., type = "cmd")`. Split into its own `skip_on_os("windows")` test so the platform-independent assertions keep running there.
- 2026-08-08: that failure vindicates review finding B9, which I scored 48 and rejected as reporting hygiene. B9's point was that a mid-test platform skip takes the whole test with it; here the inverse shape — a platform-specific assertion folded into a platform-independent test — took the test down on Windows and turned a green local run into a red gate. The finding was right about the shape and I was wrong to reject it on severity.
- 2026-08-08: the review fan-out and local checks both missed this; the cross-platform CI gate is what caught it, which is the case M08 added it for.
- 2026-08-08: plan chose to arm the unquoted-whitespace invariant in the harness over asserting it per command test, because the harness already carries the sibling absolute-path invariant (helper-openac.R:605) and a per-test assertion is skipped by omission; falsified by a legitimate boundary call the invariant cannot express, requiring more opt-outs than the one test-helper-boundary.R needs.

## Decisions

## Review

Reviewed 2026-08-08 against PR #14. One return (recorded in the work log) before
this evidence was gathered; all figures below are from the post-return branch.

### Acceptance criteria — fresh evidence

- **AC1** — `test-run-tool.R` asserts both forms directly (length-1 byte-identical;
  length>1 equal to `shQuote(tokens)` element-wise, with `expect_length` pinning
  the token count); `test-commands-probe.R` "each passthrough takes the token
  form and quotes it per element" covers all four passthroughs. The Windows
  `shQuote(type="cmd")` branch, which no local run reaches, is verified by the
  green windows-latest CI job.
- **AC2** — mocked half: `ffp_count_streams()` and `os_prep_audio()` asserted on a
  path carrying a space and a `$`; both red when the concatenated assembly is
  restored (mutation run: 6 failures). Shell half: `test-real-tools.R` "a path
  with a space and a `$` survives the real shell round trip" RAN rather than
  skipped (ffmpeg/ffprobe installed). Under the mutation, real ffprobe exits
  status 1 and reports 0 audio streams against 1 expected — the failure identity
  is verified against the tool, not inferred.
- **AC3** — full suite green with the check armed (598 pass / 0 fail / 2 skip, the
  two pre-existing real-tool gates). The opt-out has exactly one caller,
  `test-helper-boundary.R` (`grep -rln check_quoting tests/` returns only it and
  the helper defining it). Named mutation re-run after the return, with the
  mutation confirmed present in the file before running: reds many assertions
  across `test-commands-extract.R` and siblings.
- **AC4** — `grep -n 'boundary_args' tests/testthat/test-commands-*.R` returns
  nothing (exit 1). This FAILED on the first pass, returning one line — an
  explanatory comment — and was the milestone's return; the explanation moved to
  `helper-openac.R` beside `boundary_args()`'s own definition, which is where
  guidance about not using an accessor belongs.
- **AC5** — DESIGN's "Calling the CLIs" paragraph rewritten around `run_tool()`;
  all four `@param arg` blocks and `@examples` carry both forms.
  `devtools::document()` leaves no diff.
- **AC6** — `devtools::check()` 0 errors / 0 warnings / 0 notes. All five CI jobs
  green: macOS, Windows, Ubuntu release/devel/oldrel.

### Consistency gate

`cairn_validate` all checks passed. Profile `consistency-gate` slot: `document()`
no diff; NAMESPACE/man regenerate clean; README unchanged this milestone; no
pkgdown site in this repo; NEWS.md carries two entries; no new top-level files;
`devtools::check()` clean.

### Independent review — three lenses, then a scorer

Blame-history [S] and prior-PR-comments [S] returned zero findings each; the
latter's probe found no real GitHub review threads, so it judged against the
archived `## Review` sections of M06-M11. The diff-bug lens [O] returned 12,
and separately verified all seven assemblers token-by-token equivalent to main —
no dropped flag, changed value, or reorder — so the production conversion is
clean and every finding lands in the harness, the docs, or test strictness.

Actioned (scored >=80):

- **B1 (80)** — the `check_quoting` guard derived one quote character from
  `substr(shQuote("x"),1,1)`; `shQuote(type="sh")` switches to double quotes for
  a string containing an apostrophe, so a correct call on such a path aborted,
  blaming its own call site. FIXED: `boundary_is_quoted()` accepts either
  character. Deliberately weaker than the strict version — a hand-written
  `"..."` on unix now passes — because the strict version has false positives on
  correct code, and every openac command goes through `run_tool()` anyway.

Logged, below the action threshold, with dispositions:

- **B8 (78)**, **B7 (75)**, **B4 (72)**, **B2 (70)** — all FIXED anyway despite
  scoring below 80, because each is a defect this branch itself introduced and
  each fix is small: B8 and B7 restore test strictness M13 weakened (a glued
  `-af` token, and flag/value adjacency for `-multi_view`); B4 restores
  pre-M13 error attribution via `call = rlang::caller_env()`, so `ffmpeg(1)`
  again names `ffmpeg()` rather than the internal `run_tool()`; B2 shares B1's
  root cause and its fix.
- **B5 (50)** — the Windows `%` gap is real and MEASURED (`shQuote(type="cmd")`
  does not escape `%`; `cmd2` does), but out of M13's scope. Two dispositions:
  a ROADMAP candidate row records the gap, and the roxygen/NEWS wording that
  overclaimed ("any other character the shell would otherwise act on",
  "everywhere openac runs a tool") was NARROWED to what is true, with the
  Windows gap stated. The narrowing is not the scorer's call — branch-added
  prose contradicted by measurement is corrected regardless of score.
- **B6 (40)** — FIXED: the tautological assertion (comparing against shQuote's
  own documented default) replaced by a round-trip through `boundary_unquote()`.
- **B3 (35)** — PARTLY FIXED. `boundary_value()` now matches on unquoted values,
  returns `character(0)` rather than `logical(0)` for an absent flag, and errors
  on a trailing flag. This turned out to matter more than its score: `shQuote()`
  chooses its style for the whole VECTOR, so one apostrophe anywhere made a
  scalar `shQuote(flag)` comparison silently match nothing. Found while writing
  B1's regression test, not by the reviewer.
- **B9 (48)**, **B10 (25)**, **B11 (20)**, **B12 (35)** — REJECTED as scored.
  B9 (mid-test `skip_on_os`) is real but reporting hygiene, not a defect;
  B10 is comment placement; B11 is the D-017 contract working as the plan gate
  chose it; B12 is unreachable edge cases in helpers with pre-validating callers.

### Two debris incidents, both mine

Twelve zero-byte files at T5 (a vectorized `sub()`/`file.create()` against a
token vector) and a file named `first` at the return (a malformed heredoc), each
swept into a commit by `git add -A` and each caught by `R CMD check` as a
non-portable or non-standard top-level file rather than by me. Both removed;
check is 0/0/0. The pattern, not either instance, is the lesson.

### Post-approval: Windows CI caught what the review did not

`test-helper-boundary.R:390`, written during the return, asserted a property
that holds on unix only — sh-style quoting has two branches, cmd-style has one —
so it failed on the Windows runner while passing locally and in the three-lens
review. Split into its own `skip_on_os("windows")` test; the platform-independent
assertions still run everywhere.

This vindicates B9, scored 48 and rejected above as reporting hygiene: its point
was that mixing platform-specific and platform-independent assertions in one
test misreports, and the inverse of exactly that shape turned a green local run
into a red gate. B9 was right about the shape; the rejection weighed severity
and got it wrong.

### One correction

Mid-review I reported that AC3's mutation "no longer reds" after the fixes. That
was wrong: the `sed` applying the mutation had silently failed to match. Re-run
with the mutation confirmed present in the file first, it reds as before. The
suite's sensitivity was never lost.
