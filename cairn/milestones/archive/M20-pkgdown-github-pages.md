# M20: A published documentation site

**Status:** done (2026-08-09, PR #22 https://github.com/jmgirard/openac/pull/22)

**Goal:** Publish openac's reference and vignettes as a pkgdown site on GitHub
Pages, rebuilt by CI on every push to the default branch.

**Outcome:** The site is live at https://jmgirard.github.io/openac/, served from
a `gh-pages` branch. `_pkgdown.yml` carries an explicit `reference:` index of
eight groups mirroring DESIGN's Function Families, covering all 37 `man/*.Rd`
topics. DESCRIPTION gained `URL:`, `BugReports:` and `Config/Needs/website:`;
`.Rbuildignore` gained `^_pkgdown\.yml$`, `^docs$`, `^pkgdown$`.
`.github/workflows/pkgdown.yaml` builds on pull requests and deploys on
non-`pull_request` events, asserting `audio.whisper` is never installed.

**Decisions:** The site publishes `CLAUDE.md` — `pkgdown:::package_mds()` skips
a hardcoded list it exposes no way to extend, and the repo is public, so the
user chose to leave it published. pkgdown as a website-only dependency is D-020.

**Review:** Two rounds. Round 1 returned it: the changelog check failed with
`NEWS.md` untouched (F5 95, F18 90). Round 2 passed all six criteria on fresh
evidence — 20/0/0 findings, none ≥80; the concurrency finding was refuted.
