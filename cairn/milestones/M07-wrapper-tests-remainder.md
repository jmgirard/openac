# M07: Wrapper testing contract — remainder and gated real invocations

- **Status:** review
- **Priority:** normal
- **Depends on:** M06
- **Driving RR:** —
- **Principles touched:** GP3, GP6, GP7, IP1

- **Branch/PR:** `m07-wrapper-tests-remainder` / PR #8 https://github.com/jmgirard/openac/pull/8

## Goal

Empty M06's deferral list and add GP7's second layer — real tool invocations
behind skip gates.

## Scope

**In:** command/URL-construction tests for the `install_*` family with network
and archive extraction mocked; OS guards on the platform-specific installers;
tests for the `*_dir` batch wrappers (enumeration, output-path derivation, GP6
skip-and-report) including the path-derivation fixes they surface;
`aw_transcribe`/`aw_transcribe_wav` against a mocked `audio.whisper`; and a
gated real-invocation layer.

**Out:** installer *dispatchers* (`install_openface()` detecting the OS and
delegating) → candidate row, since they add exports and amend DESIGN's
`_win`/`_mac` convention and so need a D-entry. GP8 runtime reporting →
candidate row. Replacing the OneDrive model URLs → candidate row. CRAN
submission → user-declared release window (D-050).

## Acceptance criteria

- [x] AC1 The deferral list in `test-command-contract.R` is empty and the test
      passes with its computed domain covering the full transitive closure — so
      every openac function that can reach `system2` records a command
      assertion, with no name exempted.
- [x] AC2 Each of `install_ffmpeg_win`, `install_openface_win`,
      `install_opensmile_win` and `install_opensmile_mac` has tests asserting the
      download URL and install path it constructs, with `utils::download.file` and
      `archive::archive_extract` mocked to record their arguments and to fail
      the test if called with an unexpected URL or destination. `install_whisper`,
      which downloads nothing, is tested for the one thing it does: delegating to
      `rlang::check_installed("audio.whisper")`. The mocks are
      the procedure that establishes no test performs a real network request or
      writes outside a per-test temporary directory.
- [x] AC3 Every installer whose name carries a `_win` or `_mac` suffix — the
      set computed from `getNamespaceExports("openac")` by suffix match, not a
      hand list — aborts with a classed `cli` condition when `Sys.info()`
      reports a different OS, tested in both directions per installer. On an OS
      with no installer for a tool, the message names the tool and states that
      no automated installer exists rather than failing silently.
- [x] AC4 For each `*_dir` wrapper, tests cover: extension matching (including
      that a file named `clip.mp4.backup.mp4` and a directory named `mp4` do not
      mis-derive), `recursive` on and off, output-path derivation for an input
      directory containing regex metacharacters, and GP6 skip-and-report when
      the per-file operation fails on one file of several. The derivation bugs
      these surface (`gsub(indir, outdir, …)` treating a path as a regex,
      unanchored `gsub(inext, …)`, and the inconsistent `paste0(inext, "$")`
      vs `paste0("\\.", inext, "$")` patterns) are fixed (D-002 permits the
      behavior change).
- [x] AC5 `aw_transcribe()` and `aw_transcribe_wav()` are tested with
      `audio.whisper`'s `predict` mocked, asserting the parameters passed
      through and the file written, with no model download and no whisper run.
- [x] AC6 `tests/testthat/test-real-tools.R` performs at least one real
      invocation per wrapped tool behind `skip_if(!check_<tool>())` and
      `skip_on_cran()`; `devtools::test()` passes both with the tools installed
      and with resolution forced to fail, and `devtools::check()` reports 0
      errors and 0 warnings with NOTEs measured against M06's recorded baseline.

## Coverage

- AC1 → T7
- AC2 → T2
- AC3 → T1
- AC4 → T3, T4
- AC5 → T5
- AC6 → T6, T8

## Tasks

- [x] T1 Add OS guards to the `_win`/`_mac` installers plus the
      no-installer-for-this-OS message; test both directions with `Sys.info()`
      mocked.
- [x] T2 Extend `helper-openac.R` with `download.file` / `archive_extract`
      mocks; write the `install_*` URL and install-path tests.
- [x] T3 Fix the `*_dir` path derivation — anchor the extension pattern, stop
      treating `indir` as a regex, derive output paths without a global `gsub`.
- [x] T4 Tests for the `*_dir` wrappers: enumeration, `recursive`, metacharacter
      directories, GP6 skip-and-report with the per-file operation mocked.
- [x] T5 Tests for `aw_transcribe`/`aw_transcribe_wav` with `audio.whisper`'s
      `predict` mocked.
- [x] T6 Write `test-real-tools.R` — one gated real invocation per tool behind
      `skip_if(!check_<tool>())` + `skip_on_cran()`.
- [x] T7 Empty the deferral list in `test-command-contract.R`; confirm the
      staleness assertion and the full-closure gate pass.
- [x] T8 Run `document()`, `test()` (both tool states), `check()`; compare
      NOTEs against M06's baseline; fix fallout.

## Work log

- 2026-08-07: created by /milestone-plan alongside M06.
- 2026-08-07: criteria audit [O] ran on M06's criteria block, which fixed this milestone's domain boundary; M07's criteria inherit its two structural repairs — literal deferral names with a staleness assertion, and a symbol-occurrence closure, since `os_extract_dir` and `aw_transcribe_dir` reach their tools via `do.call(what = …)` and are invisible to a call-head walk, which would let AC1's "deferral list empty" gate pass with them outside the domain.
- 2026-08-07: review — three lenses returned 23 findings; scorer actioned 3 (D9 90, D1 88, D2 82), all fixed on the branch, 20 logged below threshold. D1 was a real defect in new code: `dir_inputs()` enumerated directories, since `list.files(recursive = FALSE)` returns them and `file.exists()` is TRUE for a directory. D2 showed the test AC4 points at could not fail. D9 was a test disconnected from the function it claimed to guard. Suite 444 pass / 0 fail / 2 skip; `check()` 1 NOTE (spelling baseline), 0 errors, 0 warnings.
- 2026-08-07: T8 — `devtools::document()` leaves the tree clean; `devtools::test()` 439 pass / 0 fail / 2 skip; `devtools::check()` `Status: 1 NOTE`, 0 errors, 0 warnings. The NOTE is M06's recorded spelling-diff baseline, measured rather than assumed: `spelling::spell_check_package()` run on this branch and on a `main` worktree both return 54 words with a byte-identical sorted list, so this branch adds no NOTE and no new word. Doc fallout: the three vignettes that call a `*_dir` wrapper each gained one sentence on the new skip-and-report behavior and the returned outcome table; the word list was re-measured after that edit and is still identical to main.
- 2026-08-07: T6 — `test-real-tools.R` runs the wrapped tools for real behind `skip_on_cran()` and a per-tool `check_*()` gate, with input media generated by ffmpeg's lavfi sources so the repo carries no binary fixture. Whisper's gate is test-local (`audio.whisper` installed AND `OPENAC_WHISPER_MODEL` naming an existing file), so no run can trigger a model download. Both suite states measured, not assumed: with the tools present, 439 pass / 0 fail / 2 skip (OpenFace and audio.whisper absent here); with resolution forced to fail — PATH stripped of the tool directory and `HOME` redirected so the recorded openSMILE config is unreachable, verified by `Sys.which()` returning empty for both — 426 pass / 0 fail / 7 skip. The `check_*()` gates are wrapped in `suppressWarnings()`: a missing tool warns by design, which is right for a user and noise in a gate expecting absence.
- 2026-08-07: T7 — the deferral list is empty and the contract test passes with the full computed closure enforced: 27 members, 0 exempt, printed from `system2_closure()` rather than assumed. The empty list is written as a named-empty vector because a bare `character()` carries `names() == NULL`, which the staleness assertion's `identical(., character())` rejects — the first run failed exactly there.
- 2026-08-07: T5 — `test-whisper-transcribe.R` covers `aw_transcribe`, `aw_transcribe_wav` and `aw_transcribe_dir` with whisper intercepted; no model is downloaded and `audio.whisper` need not be installed. The mock must go in openac's namespace, not in `stats`: `predict` is an imported binding resolved through openac's imports environment, so rebinding `stats::predict` left the real generic in play and dispatch failed with "no applicable method" — verified by running it both ways. Two test-side corrections came from observed failures rather than assumption: the ffmpeg call is selected by tool rather than by a positional index, and the injected batch failure was moved off the leading stream count, which `aw_transcribe()` catches and turns into a no-audio skip rather than a failure. 426 pass, 0 fail.
- 2026-08-07: T3+T4 (committed together — a bug fix and its regression tests belong in one commit; minor amendment, no criterion changed) — `R/utils.R` gains `dir_inputs()`, `dir_outputs()`, `dir_walk()` and `with_progress_mode()`, and all five `*_dir` wrappers now route through them. Both derivation defects were reproduced by running the pre-M07 code against the new fixture: with `indir` = `study(1)+raw.data`, `gsub(indir, outdir, infiles)` matched nothing — `(1)` is a capture group and `+` a quantifier — so outputs were derived back INTO the input directory, and `b.mp4.backup.mp4` became `b.csv.backup.csv`; `notes.notmp4` was also enumerated for `inext = "mp4"`. GP6 skip-and-report added per the gate: a per-file failure warns naming the file and returns in a per-file outcome table. 384 pass, 0 fail.
- 2026-08-07: T2 — each downloading installer now has its URL, extraction directory and registered tool location asserted exactly, the location read back from the config file `set_program()` wrote rather than recomputed by the test; `install_openface_win`'s four patch-expert fetches are pinned by destination filename. Failure direction covered per installer: a non-zero `download.file` status warns, returns FALSE, extracts nothing and records no location. `install_whisper` is pinned to its `rlang::check_installed("audio.whisper")` delegation. `test-helper-boundary.R` asserts the two install-time fakes actually intercept, so AC2's no-real-network claim rests on a test rather than on assumption. 342 pass, 0 fail.
- 2026-08-07: T1 — `require_os()` guards the four suffixed installers, aborting with class `openac_wrong_os`; it resolves the sibling installer for the running platform from `getNamespaceExports()`, so the message either names the sibling or states no automated installer exists for that tool there. `test-installers.R` computes the guarded set from exports and asserts a fixture exists for each member, so a future suffixed installer cannot slip through untested. Verified by mutation: deleting `install_ffmpeg_win()`'s guard turns three tests red (the wrong-platform assertion fails on a different error class, not on a bare failure), and restoring it returns 294 pass / 0 fail. The inert unexported `install_openface_mac()` was deleted (gate decision); helpers `local_fake_os()`, `local_fake_downloads()` and `local_fake_data_dir()` added — the download/extract fakes are the T2 harness landed early because T1's right-platform direction needs them.
- 2026-08-07: amendment (substantive, gated) — AC2 dropped `install_openface_mac` and re-scoped `install_whisper`: read against `R/programs_install.R`, `install_whisper()` is `rlang::check_installed("audio.whisper")` and constructs no URL or install path, and `install_openface_mac()` is unexported and assigns a bash script to a local variable it never runs, so the URL/install-path assertion was unmeetable for both. AC2 now names the four installers that download, and pins `install_whisper` to its delegation.
- 2026-08-07: implementation gate — user chose to delete the inert `install_openface_mac()` (unexported, no caller reachable), with a real macOS OpenFace installer captured as a ROADMAP candidate; `*_dir` GP6 skip-and-report returns an invisible per-file outcome table (D-002 permits the return-shape change); real-tool media is generated at run time by ffmpeg's lavfi sources rather than committed as a fixture, and whisper's gate requires both `audio.whisper` and an already-cached model so no test downloads one.
- 2026-08-07: plan gate chose OS guards here over installer dispatchers because guards are a correctness fix for a verified defect (no `Sys.info()` or `.Platform` check exists anywhere in `R/programs_install.R`, so `install_opensmile_win()` on macOS extracts Windows binaries and reports success) while dispatchers add exports and amend a stated DESIGN convention; falsified by the user preferring a single platform-aware entry point over the suffixed family.

## Decisions

## Review

_2026-08-07, PR #8. Branch cut from `main` at de8ec49; `main` had not moved
(`git rev-list HEAD..origin/main` = 0), so no merge-forward was needed and every
measurement below is against a current branch._

**Consistency gate.** `cairn_validate` exits 0 — all 16 CHECKs PASS and all 8
advisories OK. No `DESIGN.md` principle changed, so `cairn_impact` does not
apply. Toolchain gate (`r-package` profile): `devtools::document()` leaves the
tree clean (no `NAMESPACE` or `man/` diff); `NAMESPACE` and `README.Rmd` are
untouched by the branch so no re-knit is owed; no `_pkgdown.yml` exists, so that
check is not applicable; `NEWS.md` carries entries for every user-visible change;
no top-level file was added, so no `.Rbuildignore` entry is owed;
`devtools::check()` reports 0 errors, 0 warnings, 1 NOTE.

**AC evidence**

- AC1 — `system2_closure()` computed live at review returns 27 members;
  `length(deferred)` is 0, so all 27 are enforced and none exempt. Run alone, the
  contract file skips its coverage gate by design ("needs the full test suite");
  in the full-suite run it executes, and the only two skips in the whole suite are
  the real-tool gates for OpenFace and audio.whisper — so the gate ran and passed.
- AC2 — `test-installers.R` 85 pass / 0 fail. Each of the four downloading
  installers asserts its URL, extraction directory and registered tool location
  with `expect_identical`, the location read back from the config file
  `set_program()` wrote; `install_whisper` is pinned to its
  `rlang::check_installed("audio.whisper")` delegation. Discrimination measured by
  mutation, not inferred: bumping `install_opensmile_win`'s pinned URL from v3.0.2
  to v3.0.3 turned the suite red on `download_urls(state)` (1 fail / 84 pass), and
  restoring it returned 85 pass. The no-real-network / no-stray-write claim rests
  on two measurements — `test-helper-boundary.R` asserts the `download.file` and
  `archive_extract` fakes actually intercept (20 pass), and this machine's real
  openac config dir was unmodified across the installer runs
  (`find -newermt '-3 minutes'` returned 0; its contents still date from 2024).
- AC3 — the guarded set is computed at review from
  `getNamespaceExports("openac")` by suffix and returns exactly
  `install_ffmpeg_win`, `install_openface_win`, `install_opensmile_mac`,
  `install_opensmile_win`; `test-installers.R` asserts that set and its fixture
  table are mutually exhaustive, so a future suffixed installer fails rather than
  going unexercised. Four per-installer test bodies run over that computed set —
  wrong platform aborts (`class = "openac_wrong_os"`, with zero downloads and zero
  extractions recorded, so the guard precedes the network), the no-installer-here
  message names the tool and `set_<tool>()`, the right platform proceeds to a
  download, and a failed download reports and installs nothing. Both message
  branches are pinned as mutually exclusive: `install_opensmile_win()` on Darwin
  names `install_opensmile_mac` and does *not* say "no automated", while
  `install_ffmpeg_win()` on Darwin says "no automated" and does not invent a
  sibling. Guard discrimination measured by mutation during implementation:
  deleting `install_ffmpeg_win()`'s guard turned three tests red, and they failed
  on a different error class rather than on bare failure.
- AC4 — `test-batch-dirs.R` 42 pass / 0 fail, over 13 test bodies covering
  extension matching (`clip.mp4.backup.mp4`, `notes.notmp4`, a directory named
  `mp4`, a leading-dot `inext`, and a metacharacter extension), `recursive` on and
  off, derivation under an input directory named `study(1)+raw.data`, an input
  outside `indir`, the empty-directory case, and GP6 skip-and-report in three
  shapes (one file of two fails, the warning names the file, all files fail). The
  derivation fixes are backed as regressions by executing the pre-M07 code against
  the same fixture: `gsub(indir, outdir, infiles)` matched nothing under that
  directory name — `(1)` is a capture group and `+` a quantifier — so outputs were
  derived back INTO the input tree, and `b.mp4.backup.mp4` became
  `b.csv.backup.csv`; `list.files(pattern = "mp4$")` also returned
  `notes.notmp4`.
- AC5 — `test-whisper-transcribe.R` 42 pass / 0 fail, asserting the five
  parameters `aw_transcribe_wav()` hands whisper (`object`, `newdata`, `type`,
  `language`, `trace`), that `whisper_args` merge, that the `.rds` holds the whole
  object and the `.csv` only `$data`, that nothing is written when no path is
  given, and that whisper is never reached on any of the four rejection branches.
  The no-download / no-whisper-run claim is measured rather than argued:
  `requireNamespace("audio.whisper")` returns FALSE on this machine, so no whisper
  code exists here to run and no model could be fetched.
- AC6 — `test-real-tools.R` gates every test on `skip_on_cran()` plus its tool's
  `check_*()`, and covers ffprobe, ffmpeg, openSMILE (single file and `_dir`),
  OpenFace and whisper. Both suite states measured. Tools installed: 439 pass / 0
  fail / 2 skip, the two skips being OpenFace and audio.whisper, which are absent
  here. Resolution forced to fail — PATH reduced to a shim dir holding only R, and
  `HOME` redirected so the recorded openSMILE config is unreachable, confirmed by
  `Sys.which()` returning empty for both `ffmpeg` and `SMILExtract` — 426 pass / 0
  fail / 7 skip, every real-tool test skipping and the mocked layer unaffected.
  `devtools::check()` re-run at review: `Status: 1 NOTE`, 0 errors, 0 warnings.
  The NOTE is M06's recorded spelling-diff baseline, established by measurement:
  `spelling::spell_check_package()` on this branch and on a `main` worktree each
  return 54 words with a byte-identical sorted list, so the branch adds no NOTE
  and no new word.

**CI.** PR #8 green on all five `check-standard` jobs: macos-latest (release),
windows-latest (release), ubuntu-latest (release, devel, oldrel-1). The Windows
job is the load-bearing one here — the new installer tests mock `Sys.info()` to
run a Windows installer's path on whatever host CI provides, and M08's two CI
failures were exactly this class.

**Independent review — three lenses plus scorer.** [O] diff-bug (18 findings),
[S] blame-history (4), [S] prior-PR-comments (1) — 23 total, scored 0-100 by a
fresh [S] scorer holding the diff and this milestone file. Three scored ≥80 and
were actioned; 20 scored below threshold and are logged below. No finding met
the return floor: AC4 names "a directory named `mp4`" and that literal case does
pass, so D1/D2 do not falsify a criterion as written, and D9 is a vacuous test
rather than a defect in user-facing behavior.

Actioned (all three fixed on the branch):

- **D9 (90)** — `aw_transcribe_wav() writes nothing when no output path is
  given` could not fail: it created `outdir` with `local_tempdir()`, never
  passed it to anything, then asserted `outdir` was empty. Rewritten to place
  the input alone in its own directory and assert that directory still holds
  only the input, so an unrequested `.rds`/`.csv` written beside the input —
  the IP1 violation the test exists to guard — now turns it red.
- **D1 (88)** — `dir_inputs()` enumerated directories. `list.files(recursive =
  FALSE)` returns directories as well as files, so a directory named
  `scenes.mp4` matched `\.mp4$` and was handed to the tool as an input;
  `file.exists()` is TRUE for a directory, so the wrappers' own input check did
  not catch it either. Fixed by filtering with `!dir.exists()`. A directory
  named plain `mp4` — the case AC4 names — could never expose this, because it
  does not match the anchored pattern at all.
- **D2 (82)** — the test covering that case passed for the wrong reason: it
  asserted directory exclusion against the `recursive = TRUE` listing, which
  omits directories for free, so the assertion could not fail. The fixture
  gained a `scenes.mp4/` directory and a new test asserts on the non-recursive
  listing, and that `of_extract_dir()` reaches the tool exactly twice. Verified
  by mutation: removing the `!dir.exists()` filter turns 10 assertions across
  four tests red; restoring it returns 444 pass / 0 fail.

Logged below threshold (20), surfaced not dropped: D3 (74) `dir_inputs()`
hard-codes `ignore.case = TRUE`, making three previously case-sensitive
wrappers case-insensitive — undocumented, and on a case-sensitive filesystem
`interview.mp4` and `interview.MP4` derive the same output path and silently
overwrite; D6 (70) four of `install_openface_win`'s five URLs are pinned only by
host prefix, so URL↔destination pairing is unasserted; D17 (56) two test-file
headers still carry M06's AC numbering; D11 (55) `aw_transcribe()`'s no-audio
skip is recorded `success = TRUE`, so a skipped file is indistinguishable from a
processed one in the outcome table; D13 (52) `install_openface_win()` leaks
`options(timeout = 300)` from two generated tests; D16 (52) `dir_walk()`'s
blanket `tryCatch` turns one clear "tool not found" abort into N warnings; D4
(48) three wrong-OS tests omit the download fake and rely on the guard alone;
D12 (45) several `expect_error()` matchers pin `stopifnot()` deparse text; D7
(42) the batch tests assume the default sequential `future` plan; D14 (40) the
`_win`/`_mac` absolutized-path divergence is asserted rather than normalized; D5
(38) `install_openface_win()` records the tool location before the patch-expert
downloads, so a partial install returns FALSE but leaves a location recorded;
D15 (38) `dir_outputs()` creates every output directory up front; B1 (35) and P1
(33) the new `fake_sys_which()` repeats the `Sys.which` fidelity gap M08's F6
logged, at a second site; D10 (34) the real-tool media helpers do not verify
ffmpeg produced a file; D8 (30) DESIGN "Known issues" is stale on two entries
M07 resolved; D18 (20) dot-prefixed files are excluded from every batch wrapper;
B3 (15) `dir_walk()`'s `parallel` default is a fragility note, not a defect; B2
(8) the `install_openface_mac()` deletion was a logged gate decision; B4 (5)
premise factually wrong — the zero-input test exists.
