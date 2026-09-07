# M21: A site that knows its version, and deploys that leave nothing behind

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the deliverable is the published documentation site users read
- **Branch/PR:** —

## Goal

Give development documentation its own home under `/dev/` so a push to the
default branch can never overwrite the released site, and make both deploy
lanes replace what they publish instead of merging onto it.

## Scope

**In:** `development: mode: auto` in `_pkgdown.yml`; a deploy tail in
`.github/workflows/pkgdown.yaml` that picks its lane from the built tree
(`docs/dev` present or not) and cleans within that lane's target — dev to
`dev/`, release to the root with `clean-exclude: dev`; a real dispatched
measurement of both lanes' cleaning against `gh-pages` under a throwaway
preview path, removed again before merge; a `NEWS.md` entry.

**Out:** the `gh-pages` root, which keeps serving M20's `0.1.0.9000` build
until the first `release: published` event rebuilds it — accepted at the plan
gate and held as a ROADMAP candidate row. The root-target case of the release
lane is not measured (AC4 states this). A version dropdown or custom
`development.version_label` → not planned; raise as a candidate if wanted.
A "Get started" vignette → the existing candidate row.

## Acceptance criteria

- [ ] AC1: `_pkgdown.yml` sets `development: mode: auto`, and with `docs/`
      removed beforehand, `pkgdown::build_site_github_pages(dest_dir = "docs",
      new_process = FALSE, install = FALSE)` on the working tree at
      DESCRIPTION's `0.1.0.9000` leaves `fs::dir_ls("docs", all = TRUE)`
      reporting exactly one entry, the directory `docs/dev`; the same command
      under `PKGDOWN_DEV_MODE=release`, again from a removed `docs/`, writes
      `docs/index.html` and no `docs/dev`. Both listings quoted in the review.
- [ ] AC2: `.github/workflows/pkgdown.yaml` decides its deploy lane from the
      built tree, not from the event: one step tests whether `docs/dev` exists
      and both deploy steps consume its output. Every `if:` key on a deploy
      step is quoted in full in the review; each gates deploy-or-not only, the
      `github.event_name != 'pull_request'` guard is retained, and none names
      a lane. The branch's own pull-request CI run logs the lane step reporting
      `dev`, and the identical test command run locally against the
      release-mode tree of AC1 reports `release`.
- [ ] AC3: A dispatched run of the dev lane's deploy step against `gh-pages` —
      the preview workflow's copy, differing from the shipped step only in
      `target-folder` — MEASURED that its cleaning is scoped to that target.
      Three items planted under the target beforehand — a stale file at the
      target root, a stale file in a nested subdirectory, and a stale
      directory — are all absent from the tree afterwards, and `git diff`
      between the `gh-pages` commits immediately before and after the run
      reports no path outside the target folder changed.
- [ ] AC4: A dispatched run with the release lane's exact deploy inputs
      (`clean: true`, `clean-exclude: dev`), differing from the shipped step
      only in `target-folder`, MEASURED that three items planted under the
      target survive byte-identical — `dev/<file>`, `dev/<sub>/<file>`, and a
      directory named `dev` nested below the target root — while a stale file
      planted elsewhere under the target is absent afterwards, and `git diff`
      across the two `gh-pages` commits reports no path outside the target
      folder changed. The review states what the run showed for the nested
      `dev` segment, and states that the shipped release step's root target is
      not what was measured.
- [ ] AC5: `NEWS.md` gains an entry telling users that development
      documentation now lives under `/dev/` on the site, that the site root
      will hold the released version from the next release onward, and that a
      page removed from the package stops being served under `/dev/`.
- [ ] AC6: Hygiene gate — this milestone's surface is covered by AC1–AC4:
      `devtools::test()` passes, `devtools::document()` produces no diff,
      `pkgdown::check_pkgdown()` passes, and `devtools::check()` reports 0
      errors, 0 warnings, and the same NOTE set it reports on `main` at the
      branch point. Both `check()` outputs quoted.
- [ ] AC7: Nothing from the measurement survives: `.github/workflows/
      pkgdown-preview.yaml` is absent from the branch tip, and
      `git ls-tree -r --name-only origin/gh-pages` lists no path under the
      preview folder. Both listings quoted.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] T1: Add `development: mode: auto` to `_pkgdown.yml` (top level, beside
      `template:`). Build twice from a removed `docs/` — once plain, once
      under `PKGDOWN_DEV_MODE=release` — capturing
      `fs::dir_ls("docs", all = TRUE)` each time.
- [ ] T2: Rewrite the deploy tail of `.github/workflows/pkgdown.yaml`
      (currently one step at the file's end with `clean: false`): add a lane
      step after "Build site" setting `lane=dev` or `lane=release` from
      `[ -d docs/dev ]` into `$GITHUB_OUTPUT`, then two deploy steps — dev
      (`folder: docs/dev`, `target-folder: dev`, `clean: true`) and release
      (`folder: docs`, `clean: true`, `clean-exclude: dev`) — each keeping the
      existing `github.event_name != 'pull_request'` guard alongside its lane
      condition. Run the lane test locally against both T1 trees.
- [ ] T3: Add `.github/workflows/pkgdown-preview.yaml`, `workflow_dispatch`
      only, copying each of T2's two deploy steps verbatim except
      `target-folder`, which points under a single preview folder. Confirm
      `.Rbuildignore`'s `^\.github$` covers it (no new entry expected).
- [ ] T4: For each lane: plant that criterion's items on `gh-pages` under the
      preview folder, record `git rev-parse origin/gh-pages` and the tree,
      dispatch the preview workflow, record the tree after, and produce the
      `git diff` between the two commits. Two runs, evidence for AC3 and AC4.
- [ ] T5: Write the `NEWS.md` entry. If `tests/spelling.Rout.save` drifts,
      regenerate with `spelling::update_wordlist(confirm = FALSE)` — never by
      hand-editing `inst/WORDLIST` (M20 lesson).
- [ ] T6: Run the verify slot on the branch, and `devtools::check()` on both
      the branch and `main` at the branch point; capture both NOTE sets.
- [ ] T7: Delete `.github/workflows/pkgdown-preview.yaml` and remove the
      preview folder from `gh-pages`; confirm with `git ls-tree -r`.

## Work log

- 2026-09-06: created by /milestone-plan.
- 2026-09-06: criteria audit ran in FULL mode (user-facing tier); returned 10 findings over 6 draft criteria. Nine applied before writing: AC1 gained `all = TRUE` and a removed-`docs/` precondition; AC2's ban narrowed to lane selection and given an enumerating procedure (quote every deploy-step `if:`); AC3 and AC4 each given three planted probes varying form and depth; AC4 gained AC3's outside-the-target diff clause; AC5's two false root claims rescoped; AC6's "any NOTE justified" replaced by the `main`-at-branch-point NOTE set. The tenth — the release lane's real target is the `gh-pages` root, which no safe measurement reaches — is carried as a stated limitation in AC4 and in Out. AC7 was added afterwards to bind the cleanup, and went back through the audit's questions.
- 2026-09-06: plan gate chose `development: mode: auto` over `mode: unreleased` and over a clean-only fix because `unreleased` forces the banner regardless of version and still lets the root flip between release and dev content, and clean-only locks in the overwrite; falsified by a pkgdown release whose `auto` resolution puts a dev version at the root.
- 2026-09-06: plan gate chose to leave the stale `gh-pages` root over hand-committing a redirect and over rebuilding it from the `v0.1.0` tag, because a redirect makes part of the deliverable an out-of-band commit no CI reproduces and the tag's tree has no `_pkgdown.yml`; falsified by evidence that the first release-lane deploy does not replace the root wholesale.
- 2026-09-06: plan gate chose real dispatched preview runs on `gh-pages` over reading the deploy action's documentation, because LESSONS records two days lost to inferred tool behavior (M13's `cmd2` claim, M16's HTTP-200 dead links); falsified by the preview target proving to differ from the shipped target on the cleaning axis.

## Decisions

## Review
