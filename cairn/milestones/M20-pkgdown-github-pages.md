# M20: A published documentation site

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m20-pkgdown-github-pages`

## Goal

Publish openac's reference and vignettes as a pkgdown site on GitHub Pages,
rebuilt by CI on every push to the default branch.

## Scope

**In:** a `_pkgdown.yml` carrying an explicit `reference:` index grouped by
DESIGN's Function Families; `URL:`/`BugReports:` in DESCRIPTION and
`Config/Needs/website: pkgdown`; `.Rbuildignore` entries for the new top-level
files; a `.github/workflows/pkgdown.yaml` that builds on pull requests and
deploys on default-branch pushes, without installing `audio.whisper`; a
`gh-pages` branch carrying a site built from this branch, with GitHub Pages
enabled against it.

**Out:** a "Get started" overview vignette → ROADMAP candidate. Rewriting or
re-enabling the three existing vignettes' chunks → not planned; they knit
`eval = FALSE` by M05's decision. Codecov/`test-coverage` workflow → existing
candidate row. Branch protection → existing candidate row.

## Acceptance criteria

- [ ] AC1 `_pkgdown.yml` exists at the repo root and carries an explicit
      `reference:` section (not pkgdown's auto-generated index), and
      `pkgdown::check_pkgdown()` runs without error against it.
- [ ] AC2 `pkgdown::build_site()` completes without error, and for every
      `.Rmd` returned by `list.files("vignettes", pattern = "[.]Rmd$")` a
      same-stem `.html` exists under `docs/articles/`.
- [ ] AC3 `DESCRIPTION` carries `URL:` naming the site and the GitHub
      repository, `BugReports:` naming the issue tracker, and
      `Config/Needs/website: pkgdown`; the site URL in `URL:` matches the
      `html_url` that `gh api repos/jmgirard/openac/pages` reports, ignoring a
      trailing slash.
- [ ] AC4 `.github/workflows/pkgdown.yaml` exists; its dependency-installation
      step is followed by a step asserting
      `!requireNamespace("audio.whisper", quietly = TRUE)`; its deploy step is
      conditioned on a non-`pull_request` event; and its run on this
      milestone's PR is green with that assert passing and with
      `audio.whisper` absent from the dependency step's own installed-package
      log.
- [ ] AC5 `gh api repos/jmgirard/openac/pages` reports a site sourced from
      `gh-pages`, and fetching `<site>/reference/index.html` and
      `<site>/articles/index.html` each returns HTTP 200 and a body naming
      openac exports and vignette titles respectively.
- [ ] AC6 `Rscript -e 'devtools::document()'` produces no diff;
      `Rscript -e 'devtools::test()'` clean; `Rscript -e 'devtools::check()'`
      reports 0 errors and 0 warnings, with every NOTE it reports quoted and
      justified in the Review section.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T2, T4
- AC4 → T5
- AC5 → T4, T5
- AC6 → T1, T2, T6

## Tasks

- [x] T1 Author `_pkgdown.yml`: bootstrap 5 template, `url:`, and an explicit
      `reference:` index whose groups mirror DESIGN's Function Families
      (program management, ffmpeg/ffprobe, `of_*`, `os_*`, `aw_*`, readers,
      re-exports). Add `.Rbuildignore` entries for `^_pkgdown\.yml$`, `^docs$`,
      `^pkgdown$`.
- [x] T2 Add `URL:`, `BugReports:` and `Config/Needs/website: pkgdown` to
      DESCRIPTION.
- [x] T3 Run `pkgdown::check_pkgdown()` then `pkgdown::build_site()`; close
      every gap either reports until both are clean.
- [ ] T4 Deploy the built site to a new `gh-pages` branch
      (`pkgdown::deploy_to_branch()`) and enable GitHub Pages against it via
      `gh api`; confirm the two AC5 URLs serve.
- [ ] T5 Add `.github/workflows/pkgdown.yaml` from the r-lib/actions pkgdown
      template, with the `audio.whisper`-absent assert placed after
      `setup-r-dependencies` (mirroring `.github/workflows/R-CMD-check.yaml`)
      and deploy gated off `pull_request`; push and confirm the PR run green.
- [ ] T6 Run `document()`, `test()`, `check()`; record NOTEs with
      justifications.

## Work log

- 2026-08-09: created by /milestone-plan.
- 2026-08-09: plan gate chose deploying the site from the milestone branch before review over enabling Pages onto a placeholder page, because the placeholder satisfies a configuration check while leaving the milestone's actual deliverable unverified by any criterion; falsified by a review finding that branch-built site content diverged from what the post-merge CI deploy produces.
- 2026-08-09: plan gate chose an explicit `reference:` index over pkgdown's auto-generated one, because the auto index makes `check_pkgdown()` pass vacuously and leaves PROFILE's "every new export gets a reference row" gate toothless; falsified by the explicit index proving to need hand-maintenance out of proportion to that gate's value.
- 2026-08-09: plan chose declaring pkgdown in `Config/Needs/website` over naming it only in the workflow, because a single-site requirement is easy to lose when a workflow is regenerated; falsified by the field going unread by the workflow's dependency step.
- 2026-08-09: T1 — `_pkgdown.yml` authored with an explicit `reference:` index of eight groups mirroring DESIGN's Function Families, covering all 37 Rd topics listed by `list.files("man", pattern = "[.]Rd$")`; `.Rbuildignore` gained `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$` and `.gitignore` gained `docs`. `devtools::test()` 0 fail / 1161 pass / 6 skip.
- 2026-08-09: T2 — DESCRIPTION gained `URL:` (site + repo), `BugReports:` and `Config/Needs/website: pkgdown` (D-020). These edits were staged before T1's checkpoint and so landed in commit e26e9ac rather than their own; recorded here rather than re-cut.
- 2026-08-09: T3 — `pkgdown::check_pkgdown()` "No problems found"; `pkgdown::build_site()` finished with no error and, for each of the three `.Rmd` returned by `list.files("vignettes", pattern = "[.]Rmd$")`, the same-stem `docs/articles/*.html` was confirmed present by a scripted `file.exists()` over that list. No index gap to close — the reference index was complete on its first run.
- 2026-08-09: criteria audit ([O], fresh context) returned five findings plus an AC1 vacuity note; findings 2, 4, 5 and the AC1 note were fixed in the wording before this file was written (assert placement pinned after dependency install; the unbounded "never compiles whisper.cpp" narrowed to the named run's install log; the unexercisable deploy-path claim narrowed to a YAML condition read plus a green PR build; an explicit `reference:` section required). Findings 1 and 3 went to the question gate as one question and were settled by the user choosing to publish during the work.

## Decisions

## Review
