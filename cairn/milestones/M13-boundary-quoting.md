# M13: Quote at the process boundary, not at the call site

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP5, GP7
- **Branch/PR:** `m13-boundary-quoting`

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

- [ ] AC1 Each of `ffmpeg()`, `ffprobe()`, `openface()`, `opensmile()` treats a
      character vector of length > 1 as one CLI token per element and hands
      `system2()` one `shQuote()`-ed element per token, with the platform's
      `type`; a length-1 argument reaches `system2()` byte-identical to today.
      Evidence: per passthrough, one mocked-boundary test of each form asserting
      the recorded vector element by element via `boundary_argv()`.
- [ ] AC2 Given a media path containing a space and a `$`, the element the
      boundary receives for that path is `shQuote()`'s rendering of the path and
      nothing else — asserted at the mocked boundary for `ffp_count_streams()`
      and `os_prep_audio()`, each test failing against the pre-milestone
      concatenated assembly. That the *shell* then delivers it as one argument is
      a separate claim, asserted in `test-real-tools.R` behind the existing
      real-binary gate by probing such a file and asserting stream counts rather
      than an error.
- [ ] AC3 `local_fake_tools()`'s fake `system2()` aborts on any boundary call
      whose argument vector has length > 1 and contains an element carrying
      whitespace that `shQuote()` would have enclosed, and the full suite passes
      with that check armed. `test-helper-boundary.R`'s deliberate raw-argv
      fixtures opt out through an explicit `local_fake_tools()` argument, and
      that argument has no other caller (`grep -rn` over `tests/`). The claim is
      over the boundary calls the suite makes, not over every branch of every
      wrapper. Verified by mutation: restoring `ffp_count_streams()`'s
      concatenated assembly (`R/use_ffprobe.R:51-56`) turns the suite red.
- [ ] AC4 No command assertion reads the collapsing accessor `boundary_args()`:
      `grep -n 'boundary_args' tests/testthat/test-commands-*.R` returns nothing.
      Its uses outside those three files are untouched.
- [ ] AC5 `cairn/DESIGN.md`'s Architecture "Calling the CLIs" paragraph
      (`:164-174`) and the four passthroughs' roxygen `@param arg` and
      `@examples` describe both accepted forms; `devtools::document()` leaves no
      uncommitted diff.
- [ ] AC6 `devtools::test()` passes and `devtools::check()` reports 0 errors, 0
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
- [ ] T3 Arm the unquoted-whitespace check in `local_fake_tools()`'s
      `fake_system2` (`helper-openac.R:596-638`), beside the existing
      absolute-path check, with the opt-out argument AC3 names; exempt
      `test-helper-boundary.R`'s raw fixtures.
- [ ] T4 Convert `ffp_count_streams()` (`R/use_ffprobe.R:51`) and
      `os_check_audio()` (`R/use_opensmile.R:114`) to token vectors; add the
      space-and-`$` regression test that fails against the old assembly.
- [ ] T5 Convert `os_prep_audio()` (`R/use_opensmile.R:174`), `aw_check_audio()`
      (`R/use_whisper.R:20`) and `aw_prep_audio()` including its `-af` filter
      branch (`R/use_whisper.R:109,129`).
- [ ] T6 Convert `of_extract()` (`R/use_openface.R:80`) and `os_extract()`
      including its `aggfile`/`lldfile`/`-instname` branches
      (`R/use_opensmile.R:331`).
- [ ] T7 Move the three `test-commands-*.R` files' assertions to
      `boundary_argv()`; drop `boundary_args()` from them.
- [ ] T8 Add the real-`ffprobe` case in `test-real-tools.R` for AC2's shell half.
- [ ] T9 Update DESIGN's Architecture paragraph and the roxygen `@param`/
      `@examples`; `devtools::document()`, `devtools::check()`, `inst/WORDLIST`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan gate chose the length-decides passthrough contract (D-017) over vector-only-with-a-dash-heuristic, vector-only-with-no-detection, and internal-helper-only, because it fixes every internal call site while breaking no existing call; falsified by a real call whose intent length cannot express — a single token carrying whitespace that must be quoted, or a multi-token vector that must reach the shell raw.
- 2026-08-08: plan gate chose to leave GP5's command-display surface out over shipping it with the quoting change, because M13 already spans 4 exported functions, 7 assemblers and 5 test files; falsified by the display surface turning out to require a different token representation than the one M13 lands.
- 2026-08-08: T1 done. MEASURED before writing anything: `system2()` does not quote `args` (a bare token vector reaches the tool split on spaces), and openac's current hand-quoted `paste0('-i "', path, '"')` loses a `$` in a path — `/tmp/a $b.mp4` was delivered as `/tmp/a .mp4`, a live bug M13 closes. `shQuote()` per token is correct on both axes and its default type is already platform-appropriate.
- 2026-08-08: T1's shell-oracle test runs the real `system2()` against a script echoing its own argv, mocking only discovery — the only test in the file that observes what the tool RECEIVES rather than what `system2()` was handed; mutation-verified (removing the quoting reds 5 assertions, the oracle among them).
- 2026-08-08: T2 done. The no-break check held as planned: `test-commands-probe.R:13-40`'s forwarding and alias tests pass unedited. One test outside that range did have to change — `passthroughs reject a non-string argument` asserted `ffprobe(c("-a","-b"))` errors, which D-017 makes valid; its multi-element case moved to a new positive assertion rather than being dropped.
- 2026-08-08: plan chose to arm the unquoted-whitespace invariant in the harness over asserting it per command test, because the harness already carries the sibling absolute-path invariant (helper-openac.R:605) and a per-test assertion is skipped by omission; falsified by a legitimate boundary call the invariant cannot express, requiring more opt-outs than the one test-helper-boundary.R needs.

## Decisions

## Review
