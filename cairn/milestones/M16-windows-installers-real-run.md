# M16: The Windows installers, actually run

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP7
- **Branch/PR:** m16-windows-installers-real-run / #16

## Goal

Run the three Windows installers against the live network on a real Windows host
so their pinned archives and OneDrive model links are known to work rather than
assumed, and fix what the run breaks.

## Scope

**In:** an opt-in integration test running `install_ffmpeg_win()`,
`install_opensmile_win()` and `install_openface_win()` into temp install and
config dirs; a dated record of what each URL in `R/programs_install.R` actually
delivered; fixes for what the run breaks — dead links, the unreported
patch-expert download failure (`:179-206`), a link that answers 200 with a
sign-in page; DESIGN's OneDrive known-issue line annotated with the measurement.

**Out:** replacing the OneDrive hosting scheme → existing candidate, promoted
only if a link measures dead. `install_opensmile_mac()` run for real on macOS →
candidate row. A macOS OpenFace installer and platform-aware dispatchers →
existing candidates. `check_openface()` shells a GUI binary (`FaceLandmarkVidMulti.exe -h`)
whose Windows behavior is itself unmeasured; if it misreports, that is a hotfix,
not this milestone.

## Acceptance criteria

- [x] AC1 A gated test runs each of the three installers into a
      `withr::local_tempdir()` under `local_fake_config()` and
      `local_fake_data_dir()`, so the host's recorded tool locations survive.
      For each it asserts the outcome AC2 records for that installer's URLs:
      all delivered → returns `TRUE` and `check_ffmpeg()` *and* `check_ffprobe()`
      (ffmpeg), `check_opensmile()`, `check_openface()` report working; any not
      delivered → the failure AC3 mandates. Gates are per-test on the running
      platform and an opt-in environment variable, never at file top level.
- [x] AC2 The Review section records, dated, for every URL matched by
      `grep -n 'https://' R/programs_install.R` outside roxygen, what the AC1 run
      received: HTTP status, and either the extracted tree's size (the three
      archives, whose temp file is `unlink()`ed) or the file's bytes (the four
      `.dat` patch experts). A URL counts as *delivered* only above a per-file
      byte floor recorded here and with content that is not HTML — an authkey
      link answering 200 with a sign-in page is the failure this milestone
      exists to catch.
- [x] AC3 Every URL AC2 records as not delivered is repointed to one that does,
      with AC1's test then passing on that host; where no replacement is found,
      it is recorded in DESIGN's known issues with the date measured dead and its
      installer made to fail naming the dead link instead of returning `TRUE`.
- [x] AC4 AC1's test asserts each of the four patch-expert files exists and
      exceeds AC2's byte floor after `install_openface_win()` returns. Today an
      absent `model/patch_experts` directory makes `download.file()` signal a
      connection error and abort the installer before its `:203` status check,
      and a sign-in page satisfies mere non-emptiness.
- [x] AC5 DESIGN's OneDrive known-issue line (dated 2026-07-11) is annotated with
      what the run measured and its date; it is replaced only for a link measured
      dead and then fixed, since "links of that shape die silently" stays true of
      a link that answered today.
- [x] AC6 `devtools::test()` passes, `devtools::check()` reports 0 errors, 0
      warnings and no note absent from a same-day check of the default branch,
      and all five `R-CMD-check` jobs are green. The opt-in variable is unset in
      CI, so a green `windows-latest` job evidences AC6 alone and never AC2.

## Coverage

- AC1 → T1, T3
- AC2 → T2
- AC3 → T3
- AC4 → T1, T3
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1 Write the gated installer test — per-test skips, temp config and data
      dirs, the three installers, the `check_*` assertions and AC4's byte-floor
      assertions; confirm it skips cleanly on macOS and clears the
      top-level-skip gate.
- [x] T2 **[Windows host]** Run it with the opt-in variable set; transcribe each
      URL's status and delivered size, and the byte floors chosen, into the
      milestone.
- [x] T3 **[Windows host]** Fix what T2 broke — dead or sign-in-page links, the
      patch-expert directory, the download-status handling in `:179-206` — and
      re-run there until AC1 passes.
- [x] T4 Annotate DESIGN's OneDrive known-issue line; `devtools::document()`,
      `devtools::test()`, `devtools::check()`; open the PR and confirm all five CI
      jobs green.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: T1 done — `test-installers-real.R`: one opt-in probe test over all nine pinned URL slots plus one test per Windows installer, gated per-test on `OPENAC_INSTALLER_RUN` and the running platform. Measurements go to the file `OPENAC_INSTALLER_LOG` names, because testthat swallows `cat()` and AC2's record has to be transcribed from the run rather than retyped.
- 2026-08-08: T2 detonated the time bomb. All four OneDrive patch-expert links answer 200 with a `login.live.com` sign-in page, and had been doing so for an unknown length of time while `install_openface_win()` returned TRUE. Separately, `install_opensmile_win()`'s pinned asset name 404s and always has — the v3.0.2 release has no `win-x64` asset. Two of the three Windows installers were broken. Full record in Review.
- 2026-08-08: T3 repointed both. The patch experts go to the Dropbox URLs OpenFace's own `download_models.ps1`/`.sh` try FIRST — openac had copied only upstream's OneDrive fallback, which is why it inherited the dead half. openSMILE goes to `opensmile-3.0.2-windows-x86_64.zip`, read off the release's API listing rather than guessed.
- 2026-08-08: T3 chose a byte floor AND a markup sniff over either alone, because each has a hole the other closes: a floor passes a large HTML error page, a sniff passes a truncated binary download. `model_byte_floor()` is a function rather than a constant so the mocked tests can lower it instead of writing 40 MB of fixture; falsified by a real model ever shipping under the floor.
- 2026-08-08: the markup sniff needs `<!--` in its needle set, not just `<!DOCTYPE`/`<html>` — MEASURED, the live.com page opens with a copyright comment. A sniff written from the obvious needles would have missed the exact page it exists for.
- 2026-08-08: T4 done — DESIGN's OneDrive known issue annotated with the measurement rather than deleted (the hazard shape is unchanged; only these four links were fixed), PR #16 opened, all five R-CMD-check jobs green. `curl` had to join Suggests: without it `R CMD check` warns `'::' or ':::' import not declared from: 'curl'`.
- 2026-08-08: plan gate chose a full real install of all three tools, OpenFace included, over ffmpeg+openSMILE with the model links probed separately, because the OneDrive authkey links are the flagged time bomb and a probe cannot show whether the extracted tree puts `model/patch_experts` where the download expects it; falsified by the OpenFace download proving too large to run on the host.
- 2026-08-08: plan chose temp config and data dirs via the existing `local_fake_config()`/`local_fake_data_dir()` helpers over letting the installers write the machine's real rappdirs config, because the run happens on the maintainer's working Windows machine and `set_*()` would overwrite the tool locations openac is actually used with there; falsified by an installer path that ignores the redirection.
- 2026-08-08: catch-up — all four tasks were committed and CI was green, but the status was left at `in-progress`; set to `review` at the start of /milestone-review.
- 2026-08-08: merged `main` (M15) into the branch. The only conflicts were M15's LF normalization of `R/programs_install.R` (resolved to the branch's content re-written LF, `--ignore-cr-at-eol` confirming main changed no content there) and the ROADMAP's M15 row and candidate list.
- 2026-08-08: review returned M16 to `in-progress` (defect return 1). Failed: the toolchain consistency gate's changelog check — `NEWS.md` has no entry for a milestone that fixed two broken installers — and the universal dependency gate, `curl` having been added to `Suggests` with no question gate and no D-entry. Every acceptance criterion's evidence held; T5–T8 carry the return.
- 2026-08-08: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 contradicting AC3's failure branch, AC2 measuring a size after the file is `unlink()`ed and hand-counting seven URLs, AC3 blind to a 200-with-sign-in-page, AC4 resting on a false claim about `download.file()`'s status and a floor of mere non-emptiness, AC5 deleting a still-true known issue — all fixed in the wording above before the gate; none became a gate question.

## Tasks (review return, 2026-08-08)

- [ ] T5 Add the `NEWS.md` entry for what users see: `install_opensmile_win()`
      was fetching an asset name the release never carried, and
      `install_openface_win()` was writing four sign-in pages where its models
      belong and reporting success; both are fixed, and a bad model download now
      fails loudly. Tell an existing OpenFace user to re-run the installer. No
      milestone numbers in user-facing text.
- [ ] T6 Hold the dependency question gate for `curl` and record the outcome as
      a D-entry (D-005, D-011 and D-016 are the precedents); if it is declined,
      replace the probe's `curl` use with `utils::download.file()` + `readBin()`
      and drop it from `Suggests`.
- [ ] T7 Fix F3: give the markup sniff one home. Delete
      `looks_like_markup()` and the probe test's inline copy, and have both call
      `openac:::starts_with_markup()`, so AC4's assertion exercises the shipped
      guard.
- [ ] T8 Fix F10: convert `download_model()`'s five base `warning()` calls to
      `cli::cli_warn()`, updating the three tests' message matchers with them.

## Decisions

## Review

### AC2 — every pinned URL, measured 2026-08-08

Host: Windows 11 build 26100, R 4.6.1 (`R version 4.6.1 (2026-06-24 ucrt)`,
`Sys.info()[["release"]]` = `10 x64`). Run via
`OPENAC_INSTALLER_RUN=true devtools::test(filter = "installers-real")`, with
`OPENAC_INSTALLER_LOG` set; every number below is transcribed from that log.

**Before the fix** — the seven URLs `grep -n 'https://' R/programs_install.R`
matched on the branch as planned:

| URL | Status | Content-Type | Delivered | Verdict |
|---|---|---|---|---|
| gyan.dev `ffmpeg-release-essentials.7z` | 206 | `application/x-7z-compressed` | 34,349,617 B | **delivered** (redirects to `ffmpeg-9.0-essentials_build.7z`) |
| GitHub `OpenFace_2.2.0_win_x64.zip` | 206 | `application/octet-stream` | 129,761,013 B | **delivered** |
| GitHub `opensmile-3.0.2-win-x64.zip` | **404** | `text/plain` | 29 B (`Not Found`) | **dead** |
| GitHub `opensmile-3.0.2-macos-armv8.zip` | 206 | `application/octet-stream` | 9,696,048 B | **delivered** |
| GitHub `opensmile-3.0.2-macos-x86_64.zip` | 206 | `application/octet-stream` | 9,833,825 B | **delivered** |
| OneDrive `resid=…53072` (0.25) | **200** | `text/html` | 34,591 B | **dead — sign-in page** |
| OneDrive `resid=…53079` (0.35) | **200** | `text/html` | 34,588 B | **dead — sign-in page** |
| OneDrive `resid=…53074` (0.50) | **200** | `text/html` | 34,585 B | **dead — sign-in page** |
| OneDrive `resid=…53070` (1.00) | **200** | `text/html` | 34,590 B | **dead — sign-in page** |

All four OneDrive links follow a redirect chain ending at
`https://login.live.com/login.srf?...` and return a page opening
`3c 21 2d 2d 20 43 6f 70 79 72 69 67 68 74` — `<!-- Copyright (`. **This is
exactly the failure the milestone was written to catch, and it had already
happened.** `download.file()` returns 0 for a 200, the four files existed and
were non-empty, and `install_openface_win()` returned `TRUE` onto four HTML
documents named `cen_patches_*.dat`. Nothing in openac could have said
otherwise. How long they had been dead is unknowable from here.

Note the sniff needle: the page opens with a comment, not `<!DOCTYPE`. A guard
looking only for a doctype would have missed the exact page it exists for.

**After the fix** — the same probe over the URLs now pinned:

| URL | Status | Content-Type | Delivered |
|---|---|---|---|
| gyan.dev `ffmpeg-release-essentials.7z` | 206 | `application/x-7z-compressed` | 34,349,617 B |
| GitHub `OpenFace_2.2.0_win_x64.zip` | 206 | `application/octet-stream` | 129,761,013 B |
| GitHub `opensmile-3.0.2-windows-x86_64.zip` | 206 | `application/octet-stream` | 9,883,105 B |
| GitHub `opensmile-3.0.2-macos-armv8.zip` | 206 | `application/octet-stream` | 9,696,048 B |
| GitHub `opensmile-3.0.2-macos-x86_64.zip` | 206 | `application/octet-stream` | 9,833,825 B |
| Dropbox `cen_patches_0.25_of.dat` | 206 | `application/binary` | 60,602,360 B |
| Dropbox `cen_patches_0.35_of.dat` | 206 | `application/binary` | 60,602,360 B |
| Dropbox `cen_patches_0.50_of.dat` | 206 | `application/binary` | 154,289,792 B |
| Dropbox `cen_patches_1.00_of.dat` | 206 | `application/binary` | 154,289,792 B |

None served markup. Statuses are 206 because the probe issues a ranged GET —
deliberately, since a HEAD can be answered by a CDN that then serves something
else, and the first bytes are what the content sniff needs.

### AC2 — what the installers extracted, and the byte floors

| Installer | Extracted tree | Floor asserted |
|---|---|---|
| `install_ffmpeg_win()` | 321,361,370 B | 30,000,000 |
| `install_opensmile_win()` | 17,800,050 B | 5,000,000 |
| `install_openface_win()` | 763,509,135 B | 80,000,000 |
| each patch expert | 60.6 MB ×2, 154.3 MB ×2 | 40,000,000 |

Tree size rather than archive size because each installer `unlink()`s its
temp file before returning — the archive cannot be measured afterwards. Floors
sit well under the smallest measured value so an upstream re-release does not
redden the suite, and orders of magnitude above a sign-in page (34 KB) or an
error body (29 B).

### AC1/AC3/AC4 — the run

With the two repointed URLs and the new `download_model()` guard, all four
tests in `test-installers-real.R` pass on this host: each installer returns
`TRUE` into a `withr::local_tempdir()` under faked config and data dirs, all
four patch experts land above the floor and are not markup, and
`check_ffmpeg()`, `check_ffprobe()`, `check_opensmile()` and `check_openface()`
all report the temp installs working. `check_openface()` shells the binary,
which loads every model file — so it is the end-to-end evidence that what was
downloaded is what OpenFace needs, not merely bytes of the right size.

The machine's own recorded tool locations were untouched: both rappdirs
directories are redirected per-test.

### AC6, partial

`devtools::check()` on this host: **0 warnings, 0 notes**, and one error — the
`OpenFace really extracts features that of_read() can read` real-tools test,
which fails identically on `main` here and is diagnosed in M15's Review. It is
a real-tools test, so it skips on every CI runner. `curl` was added to
`Suggests` for the URL probe; without it `R CMD check` warns
`'::' or ':::' import not declared from: 'curl'`.

All five `R-CMD-check` jobs green on #16 (run 31286091312, 2026-08-08):
`macos-latest (release)`, `ubuntu-latest (devel)`, `ubuntu-latest (oldrel-1)`,
`ubuntu-latest (release)`, `windows-latest (release)`. `OPENAC_INSTALLER_RUN`
is unset there, so all four of `test-installers-real.R`'s tests skipped on
every runner — the green `windows-latest` job evidences AC6 alone and never
AC2's measurements, exactly as AC6 says.

### Review pass 1 — 2026-08-08, on macOS, branch merged with `main` (M15)

**What was verified fresh here, and what was not.** The AC2/AC3/AC4 live-network
measurements are host-bound: they were transcribed from the Windows run recorded
above and cannot be reproduced from macOS. What review re-derived by command
today is everything else, plus AC2's completeness claim.

- AC1 — `devtools::test()`: 653 pass, 0 fail, 6 skips. All four
  `test-installers-real.R` tests skip per-test with their stated reasons, and
  `test-zzz-command-contract.R`'s `top_level_skips(test_path("."))` guard passes
  over the new file — so the gates are per-test and not at file top level, which
  is the half of AC1 a non-Windows host can answer.
- AC2 — completeness re-derived rather than recalled: `grep -n 'https://'
  R/programs_install.R` outside roxygen matches 8 lines building 9 distinct URLs
  (`:391` branches on `arch`), and the probe's `urls` vector holds exactly those
  9. The statuses and sizes remain the Windows run's record.
- AC3/AC4 — the repointed URLs and the `download_model()` guard are in the tree,
  covered by three mocked regression tests that each fail on `main`.
- AC5 — DESIGN's OneDrive entry is annotated with the measurement and its date,
  and retained rather than deleted.
- AC6 — `devtools::check()` here: **0 errors, 0 warnings, 0 notes**.
  `devtools::document()` produces no diff. Five CI jobs green on #16.

**Consistency gate.** `cairn_validate`: all 16 checks PASS, exit 0. Toolchain
slot: `document()` no-diff ✓; generated files ✓; README untouched, so no re-knit
✓; no `_pkgdown.yml`, a clean no-op; no new top-level files; `check()` clean ✓.
**The changelog check FAILS** — `NEWS.md` carries no entry, though two of three
Windows installers went from broken to fixed and `install_openface_win()` changed
from returning `TRUE` onto garbage to returning `FALSE` with a warning.

**Governance.** `curl` was added to `Suggests` with no `DECISIONS.md` entry and
an empty milestone `## Decisions` section. Dependency changes are never
unilateral — they take a question gate and a D-entry — and this repo has three
precedents (D-005 `tibble`, D-011 `withr` test-only, D-016 declining tidymedia).

**Fresh-context review.** Three distinct-evidence lenses ([O] diff-bug, [S]
blame-history, [S] prior-PR-comments) then an [S] scorer that generated none of
them: 25 findings, 2 scoring ≥80 and actioned, 23 logged below threshold.

Actioned (≥80):

- **F3 (85)** — the markup sniff is implemented three times (production
  `starts_with_markup()`, the test file's `looks_like_markup()`, and an inline
  copy in the probe test), all hard-coding the same five hex needles, so AC4's
  real-run assertion exercises a copy rather than the shipped guard and the two
  can silently diverge. The prior-review lens raised this independently against
  `milestones/archive/M09-harness-hardening.md`, whose Outcome was collapsing
  exactly this shape — a harness copy of a detection rule diverging from
  production — into one shared function.
- **F10 (80)** — `download_model()` is entirely new code and uses base
  `warning()` five times where DESIGN's Conventions mandate `cli::cli_warn()`
  for new code; the neighbouring `require_os()` in the same file uses
  `cli::cli_abort()`, and the new tests match on substrings a later cli
  conversion would break.

Below threshold, logged not actioned (score in brackets): F8 [78] `curl` with no
D-entry and F9 [78] no NEWS entry — both scored just under the bar but are
independently gate/governance failures above, which is how they are being
handled; F12 [72] the model loop now aborts on the first failure where the
historical `any(status1..4)` block attempted all four; F6 [70] the probe asserts
status and non-markup but never AC2's byte floor; F1 [68] and F2 [68] the sniff
matches markup anywhere in 512 bytes and across nibble boundaries rather than at
the prefix; F16 [65] a connection error warns twice; F7 [65] the probe hand-types
the nine URLs instead of reading `openface_patch_experts`; F17 [62] the probe
sets no timeout and can pull a full body into memory; F15 [60] `dir.create()`
failure returns bare `FALSE`; F18 [55] six helpers sit in the test file rather
than `helper-openac.R`; F5 [52] `>=` in code vs `>` in test; F19 [52]
`record_measurement()` appends with no run delimiter; F21 [50] `%||%` needs
R ≥ 4.4 with no declared floor; F13 [45] and F20 [45]; F11 [42]
`set_openface()` runs before the models; F25 [35]; F4 [35]; F14 [32]; F22 [22];
F23 [22]; F24 [15].

**Disposition: returned to `in-progress`.** The changelog gate failed and the
dependency question gate was never held; neither is review's to settle.
