# M16: The Windows installers, actually run

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP7
- **Branch/PR:** —

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

- [ ] AC1 A gated test runs each of the three installers into a
      `withr::local_tempdir()` under `local_fake_config()` and
      `local_fake_data_dir()`, so the host's recorded tool locations survive.
      For each it asserts the outcome AC2 records for that installer's URLs:
      all delivered → returns `TRUE` and `check_ffmpeg()` *and* `check_ffprobe()`
      (ffmpeg), `check_opensmile()`, `check_openface()` report working; any not
      delivered → the failure AC3 mandates. Gates are per-test on the running
      platform and an opt-in environment variable, never at file top level.
- [ ] AC2 The Review section records, dated, for every URL matched by
      `grep -n 'https://' R/programs_install.R` outside roxygen, what the AC1 run
      received: HTTP status, and either the extracted tree's size (the three
      archives, whose temp file is `unlink()`ed) or the file's bytes (the four
      `.dat` patch experts). A URL counts as *delivered* only above a per-file
      byte floor recorded here and with content that is not HTML — an authkey
      link answering 200 with a sign-in page is the failure this milestone
      exists to catch.
- [ ] AC3 Every URL AC2 records as not delivered is repointed to one that does,
      with AC1's test then passing on that host; where no replacement is found,
      it is recorded in DESIGN's known issues with the date measured dead and its
      installer made to fail naming the dead link instead of returning `TRUE`.
- [ ] AC4 AC1's test asserts each of the four patch-expert files exists and
      exceeds AC2's byte floor after `install_openface_win()` returns. Today an
      absent `model/patch_experts` directory makes `download.file()` signal a
      connection error and abort the installer before its `:203` status check,
      and a sign-in page satisfies mere non-emptiness.
- [ ] AC5 DESIGN's OneDrive known-issue line (dated 2026-07-11) is annotated with
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

- [ ] T1 Write the gated installer test — per-test skips, temp config and data
      dirs, the three installers, the `check_*` assertions and AC4's byte-floor
      assertions; confirm it skips cleanly on macOS and clears the
      top-level-skip gate.
- [ ] T2 **[Windows host]** Run it with the opt-in variable set; transcribe each
      URL's status and delivered size, and the byte floors chosen, into the
      milestone.
- [ ] T3 **[Windows host]** Fix what T2 broke — dead or sign-in-page links, the
      patch-expert directory, the download-status handling in `:179-206` — and
      re-run there until AC1 passes.
- [ ] T4 Annotate DESIGN's OneDrive known-issue line; `devtools::document()`,
      `devtools::test()`, `devtools::check()`; open the PR and confirm all five CI
      jobs green.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan gate chose a full real install of all three tools, OpenFace included, over ffmpeg+openSMILE with the model links probed separately, because the OneDrive authkey links are the flagged time bomb and a probe cannot show whether the extracted tree puts `model/patch_experts` where the download expects it; falsified by the OpenFace download proving too large to run on the host.
- 2026-08-08: plan chose temp config and data dirs via the existing `local_fake_config()`/`local_fake_data_dir()` helpers over letting the installers write the machine's real rappdirs config, because the run happens on the maintainer's working Windows machine and `set_*()` would overwrite the tool locations openac is actually used with there; falsified by an installer path that ignores the redirection.
- 2026-08-08: criteria audit ([O], fresh context) returned findings on all five drafted criteria — AC1 contradicting AC3's failure branch, AC2 measuring a size after the file is `unlink()`ed and hand-counting seven URLs, AC3 blind to a 200-with-sign-in-page, AC4 resting on a false claim about `download.file()`'s status and a floor of mere non-emptiness, AC5 deleting a still-true known issue — all fixed in the wording above before the gate; none became a gate question.

## Decisions

## Review
