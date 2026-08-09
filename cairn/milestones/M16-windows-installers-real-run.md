# M16: The Windows installers, actually run

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP7
- **Branch/PR:** m16-windows-installers-real-run

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
- [ ] AC6 `devtools::test()` passes, `devtools::check()` reports 0 errors, 0
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
- [ ] T4 Annotate DESIGN's OneDrive known-issue line; `devtools::document()`,
      `devtools::test()`, `devtools::check()`; open the PR and confirm all five CI
      jobs green.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: T1 done — `test-installers-real.R`: one opt-in probe test over all nine pinned URL slots plus one test per Windows installer, gated per-test on `OPENAC_INSTALLER_RUN` and the running platform. Measurements go to the file `OPENAC_INSTALLER_LOG` names, because testthat swallows `cat()` and AC2's record has to be transcribed from the run rather than retyped.
- 2026-08-08: T2 detonated the time bomb. All four OneDrive patch-expert links answer 200 with a `login.live.com` sign-in page, and had been doing so for an unknown length of time while `install_openface_win()` returned TRUE. Separately, `install_opensmile_win()`'s pinned asset name 404s and always has — the v3.0.2 release has no `win-x64` asset. Two of the three Windows installers were broken. Full record in Review.
- 2026-08-08: T3 repointed both. The patch experts go to the Dropbox URLs OpenFace's own `download_models.ps1`/`.sh` try FIRST — openac had copied only upstream's OneDrive fallback, which is why it inherited the dead half. openSMILE goes to `opensmile-3.0.2-windows-x86_64.zip`, read off the release's API listing rather than guessed.
- 2026-08-08: T3 chose a byte floor AND a markup sniff over either alone, because each has a hole the other closes: a floor passes a large HTML error page, a sniff passes a truncated binary download. `model_byte_floor()` is a function rather than a constant so the mocked tests can lower it instead of writing 40 MB of fixture; falsified by a real model ever shipping under the floor.
- 2026-08-08: the markup sniff needs `<!--` in its needle set, not just `<!DOCTYPE`/`<html>` — MEASURED, the live.com page opens with a copyright comment. A sniff written from the obvious needles would have missed the exact page it exists for.
- 2026-08-08: plan gate chose a full real install of all three tools, OpenFace included, over ffmpeg+openSMILE with the model links probed separately, because the OneDrive authkey links are the flagged time bomb and a probe cannot show whether the extracted tree puts `model/patch_experts` where the download expects it; falsified by the OpenFace download proving too large to run on the host.
- 2026-08-08: plan chose temp config and data dirs via the existing `local_fake_config()`/`local_fake_data_dir()` helpers over letting the installers write the machine's real rappdirs config, because the run happens on the maintainer's working Windows machine and `set_*()` would overwrite the tool locations openac is actually used with there; falsified by an installer path that ignores the redirection.
- 2026-08-08: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 contradicting AC3's failure branch, AC2 measuring a size after the file is `unlink()`ed and hand-counting seven URLs, AC3 blind to a 200-with-sign-in-page, AC4 resting on a false claim about `download.file()`'s status and a floor of mere non-emptiness, AC5 deleting a still-true known issue — all fixed in the wording above before the gate; none became a gate question.

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

The five `R-CMD-check` jobs are still to run — the PR is not open yet, because
`gh` is unauthenticated on this host.
