# M20: A published documentation site

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m20-pkgdown-github-pages` · https://github.com/jmgirard/openac/pull/22

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

- [x] AC1 `_pkgdown.yml` exists at the repo root and carries an explicit
      `reference:` section (not pkgdown's auto-generated index), and
      `pkgdown::check_pkgdown()` runs without error against it.
- [x] AC2 `pkgdown::build_site()` completes without error, and for every
      `.Rmd` returned by `list.files("vignettes", pattern = "[.]Rmd$")` a
      same-stem `.html` exists under `docs/articles/`.
- [x] AC3 `DESCRIPTION` carries `URL:` naming the site and the GitHub
      repository, `BugReports:` naming the issue tracker, and
      `Config/Needs/website: pkgdown`; the site URL in `URL:` matches the
      `html_url` that `gh api repos/jmgirard/openac/pages` reports, ignoring a
      trailing slash.
- [x] AC4 `.github/workflows/pkgdown.yaml` exists; its dependency-installation
      step is followed by a step asserting
      `!requireNamespace("audio.whisper", quietly = TRUE)`; its deploy step is
      conditioned on a non-`pull_request` event; and its run on this
      milestone's PR is green with that assert passing and with
      `audio.whisper` absent from the dependency step's own installed-package
      log.
- [x] AC5 `gh api repos/jmgirard/openac/pages` reports a site sourced from
      `gh-pages`, and fetching `<site>/reference/index.html` and
      `<site>/articles/index.html` each returns HTTP 200 and a body naming
      openac exports and vignette titles respectively.
- [x] AC6 `Rscript -e 'devtools::document()'` produces no diff;
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
- [x] T4 Deploy the built site to a new `gh-pages` branch
      (`pkgdown::deploy_to_branch()`) and enable GitHub Pages against it via
      `gh api`; confirm the two AC5 URLs serve.
- [x] T5 Add `.github/workflows/pkgdown.yaml` from the r-lib/actions pkgdown
      template, with the `audio.whisper`-absent assert placed after
      `setup-r-dependencies` (mirroring `.github/workflows/R-CMD-check.yaml`)
      and deploy gated off `pull_request`; push and confirm the PR run green.
- [x] T6 Run `document()`, `test()`, `check()`; record NOTEs with
      justifications.

## Work log

- 2026-08-09: created by /milestone-plan.
- 2026-08-09: plan gate chose deploying the site from the milestone branch before review over enabling Pages onto a placeholder page, because the placeholder satisfies a configuration check while leaving the milestone's actual deliverable unverified by any criterion; falsified by a review finding that branch-built site content diverged from what the post-merge CI deploy produces.
- 2026-08-09: plan gate chose an explicit `reference:` index over pkgdown's auto-generated one, because the auto index makes `check_pkgdown()` pass vacuously and leaves PROFILE's "every new export gets a reference row" gate toothless; falsified by the explicit index proving to need hand-maintenance out of proportion to that gate's value.
- 2026-08-09: plan chose declaring pkgdown in `Config/Needs/website` over naming it only in the workflow, because a single-site requirement is easy to lose when a workflow is regenerated; falsified by the field going unread by the workflow's dependency step.
- 2026-08-09: T1 — `_pkgdown.yml` authored with an explicit `reference:` index of eight groups mirroring DESIGN's Function Families, covering all 37 Rd topics listed by `list.files("man", pattern = "[.]Rd$")`; `.Rbuildignore` gained `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$` and `.gitignore` gained `docs`. `devtools::test()` 0 fail / 1161 pass / 6 skip.
- 2026-08-09: T2 — DESCRIPTION gained `URL:` (site + repo), `BugReports:` and `Config/Needs/website: pkgdown` (D-020). These edits were staged before T1's checkpoint and so landed in commit e26e9ac rather than their own; recorded here rather than re-cut.
- 2026-08-09: T3 — `pkgdown::check_pkgdown()` "No problems found"; `pkgdown::build_site()` finished with no error and, for each of the three `.Rmd` returned by `list.files("vignettes", pattern = "[.]Rmd$")`, the same-stem `docs/articles/*.html` was confirmed present by a scripted `file.exists()` over that list. No index gap to close — the reference index was complete on its first run.
- 2026-08-09: T4 — `pkgdown::deploy_to_branch()` first failed on `library(openac)` (the package was not installed locally; `devtools::install()` fixed it) and, on the run before that, surfaced that pkgdown renders `CLAUDE.md` into the site — gated to the user, who chose to leave it published (see Decisions). Deploy pushed `gh-pages` at 964b583 and enabled Pages; `gh api repos/jmgirard/openac/pages` reports `source.branch: gh-pages`, `status: built`, `html_url: https://jmgirard.github.io/openac/`, matching DESCRIPTION's `URL:`.
- 2026-08-09: T4 verification — `curl` returned HTTP 200 for the site root, `reference/index.html` and `articles/index.html`; the reference page's body was grepped for six exports spanning every index group (`os_extract_dir`, `aw_transcribe`, `of_read`, `find_program`, `ffp_count_streams`, `handlers`), all present, and the articles page carries all three vignette titles.
- 2026-08-09: T5 — `.github/workflows/pkgdown.yaml` added with `dependencies: '"hard"'` and the `audio.whisper` assert placed after `setup-r-dependencies`; PR #22 opened. All six checks green (run 31325889814, job 93276085336). Step conclusions read from the jobs API: `Confirm audio.whisper is absent` success, `Deploy to GitHub pages` **skipped** on the `pull_request` event. The assert step's `installed.packages()` output listed 108 packages including `openac` and not `audio.whisper`; pak's resolution JSON names the `Remotes:` ref, which is resolution rather than installation.
- 2026-08-09: T6 — first `devtools::document()` regenerated `man/openac-package.Rd` (roxygen picked the new `URL:`/`BugReports:` into a Useful-links block), committed; the re-run left `git status --porcelain` empty, so document() is no-diff. `devtools::test()` 0 fail / 1161 pass / 6 skip. `devtools::check()` **0 errors, 0 warnings, 0 notes** — no NOTE to justify, and the `--as-cran` URL check passed because the site was already live from T4.
- 2026-08-09: all six tasks done; status → review. PR #22.
- 2026-08-09: criteria audit ([O], fresh context) returned five findings plus an AC1 vacuity note; findings 2, 4, 5 and the AC1 note were fixed in the wording before this file was written (assert placement pinned after dependency install; the unbounded "never compiles whisper.cpp" narrowed to the named run's install log; the unexercisable deploy-path claim narrowed to a YAML condition read plus a green PR build; an explicit `reference:` section required). Findings 1 and 3 went to the question gate as one question and were settled by the user choosing to publish during the work.
- 2026-08-09: review round 1 — all six acceptance criteria passed with fresh evidence, `cairn_validate` exit 0, but the profile consistency-gate's **changelog check FAILED**: `NEWS.md` is untouched by the branch while the milestone ships user-visible changes (a public docs site, and `BugReports:`). Independent review scored 18 findings; the only two at or above 80 (F5 95, F18 90) are that same missing NEWS entry. Status → `in-progress`. Defect-return count for M20: 1.

## Decisions

### 2026-08-09: The site publishes `CLAUDE.md`, and that is accepted

`pkgdown:::package_mds()` renders every top-level `*.md` except a hardcoded
list (`README`, `LICENSE`/`LICENCE`, `NEWS`, `cran-comments`, the two GitHub
templates), and exposes no configuration to extend it — so `CLAUDE.md` builds
to `CLAUDE.html` and its text enters `search.json`. Not a disclosure: the repo
is public (`isPrivate: false`, observed 2026-08-09), so the file was already
readable on GitHub. **Decision (user, at the implementation gate): leave it
published.** Considered and rejected: (1) moving the file to
`.claude/CLAUDE.md` — a supported project-memory location that pkgdown does
not scan, but `cairn_validate.py:74` reads `<root>/CLAUDE.md` and
`claude_section_line_count()` returns `None` for a missing file (measured), so
the 30-line cairn-section cap check would pass vacuously forever; (2)
deleting the built page after each build — it would have to run locally and in
CI, leaves the search index to rebuild, and breaks silently whenever pkgdown
changes its build order. Revisit if pkgdown ever gains an exclusion option, or
if the page confuses a real user.

## Review

Round 1 — 2026-08-09. Branch at 2d651bd, `origin/main` at eddcfcd (merge-base
equal, nothing to merge in).

### Acceptance-criteria evidence

- AC1 — `grep -c '^reference:' _pkgdown.yml` = 1, so the index is explicit
  rather than pkgdown's auto-generated one; `pkgdown::check_pkgdown()`
  "✔ No problems found."
- AC2 — `docs/` removed, `pkgdown::build_site(preview = FALSE)` re-run: exit
  status 0, zero `Error`/`Warning` lines in its log. A scripted `file.exists()`
  over `list.files("vignettes", pattern = "[.]Rmd$")` found the same-stem
  `docs/articles/*.html` for all three.
- AC3 — DESCRIPTION lines 9/10/34 carry `URL:`, `BugReports:` and
  `Config/Needs/website: pkgdown`. First `URL:` element compared to
  `gh api repos/jmgirard/openac/pages --jq .html_url` with trailing slashes
  stripped: equal (`https://jmgirard.github.io/openac`).
- AC4 — run 31325889814 job 93276850318/93276085336. Step conclusions from the
  jobs API: `Confirm audio.whisper is absent` success, `Deploy to GitHub pages`
  **skipped** on the `pull_request` event. The assert step's
  `installed.packages()` listed 108 packages, `openac` present,
  `audio.whisper` absent. Workflow source confirms the assert sits after
  `setup-r-dependencies` and the deploy carries
  `if: github.event_name != 'pull_request'`.
- AC5 — `gh api .../pages`: `source.branch=gh-pages`, `status=built`,
  `html_url=https://jmgirard.github.io/openac/`. `curl` → HTTP 200 for both
  `reference/index.html` and `articles/index.html`; the reference body carries
  all six probed exports spanning every index group, the articles body all
  three vignette titles.
- AC6 — `devtools::document()` left `git status --porcelain -- man NAMESPACE`
  empty; `devtools::test()` 0 fail / 1161 pass / 6 skip; `devtools::check()`
  **0 errors, 0 warnings, 0 notes** — no NOTE to justify.

### Consistency gate

- `cairn_validate.py` exit 0 — 16 PASS, 8 advisory OK, no FAIL.
- `cairn_impact.py` not run: `Principles touched:` is `—` and the diff changes
  no IP/GP.
- Profile `consistency-gate` slot: `document()` no-diff ✔ · generated files not
  hand-edited ✔ · README.Rmd/README.md untouched by the diff ✔ ·
  `check_pkgdown()` passes ✔ · new top-level file `_pkgdown.yml` has its
  `.Rbuildignore` entry and `check()` reports no non-standard-file NOTE ✔ ·
  `check()` clean ✔ · **changelog entry — FAIL** (see below).

### Independent review — three lenses, then a scorer

[O] diff-bug 17 findings · [S] blame-history 0 ("no evidence that this diff
silently undoes, contradicts or resurrects any past deliberate decision") ·
[S] prior-PR-comments 1 (the `gh api .../pulls/comments` probe returned `[]`,
so the thread walk was skipped; its finding came from the archived `## Review`
sections). 18 scored by a fresh [S] scorer holding the diff and this plan.

**Actioned (≥80), both the same defect:**

- **F5 (95) — no `NEWS.md` entry, contrary to PROFILE's consistency-gate.**
  The milestone ships two user-visible things: a documentation website at a new
  URL, and `BugReports:` telling users where to report bugs. Neither appears in
  the development-version section of `NEWS.md`.
- **F18 (90) — the same miss, as a regression of a gate M16 already enforced.**
  M16's archived review: "Two passes. The first returned the milestone — the
  changelog gate failed and the dependency gate had never been held."

Triage: **fix now**, via the return below. This is also what the consistency
gate caught independently, so it returns the milestone on that ground alone.

**Logged below the action bar (16 findings, none actioned).** Two the scorer
rated arguable are worth a candidate row rather than silence:

- F3 (70) `clean: false` on the deploy step never deletes removed pages, so a
  renamed export leaves its old reference page served indefinitely.
- F9 (65) `_pkgdown.yml` sets no `development: mode`, so once a release exists,
  dev-version docs overwrite the release site at the root.
- F8 (50) the CLAUDE.md decision names `search.json` but the deployed branch
  also carries `llms.txt`, which likewise contains top-level page content.
- F1 (45) the deploy `if` tests the event, not the ref, so a
  `workflow_dispatch` on any branch can publish it.
- F12 (35) no `paths:` filter, so every PR runs a full site build.
- F2 (30) restates the branch-vs-CI build divergence the plan gate already
  recorded and accepted as its falsifier.
- F6 (30) README gains no site link or badge.
- F10 (30) deploy action pinned to a mutable tag while the job holds
  `contents: write` — the repo's existing house style.
- F4 (25) `.gitignore` `docs` is unanchored; the scorer measured `usethis`
  writing the same unanchored pattern, so the premise fails.
- F11 (25) pak still *resolves* the `Remotes:` ref it never installs — the same
  exposure `R-CMD-check.yaml` already carries.
- F7 (15) the CLAUDE.md decision sits in the milestone file, which the
  tracking rules say is exactly where a milestone-local decision belongs.
- F13 (12) `^pkgdown\$` matches nothing today — planned in T1 as
  forward-looking.
- F15 (10), F16 (10) cosmetic (index group casing; DESCRIPTION whitespace).
- F14 (5) pre-existing `.DS_Store`; F17 (5) a mid-review snapshot of this
  section being empty, already stale.

### Gate failure — round 1 returns

The profile's `consistency-gate` changelog check fails: `NEWS.md` is untouched
by this branch (`git diff --name-only origin/main..HEAD` lists it not at all)
while the milestone ships user-visible changes. Status → `in-progress`.

All six acceptance criteria passed on the evidence above, recorded at 2d651bd.
**Round 2 must re-run AC6** — the fix edits `NEWS.md`, which `R CMD check`
parses, so AC6's `check()` evidence does not survive the change. AC1–AC5 are
untouched by a changelog edit.
