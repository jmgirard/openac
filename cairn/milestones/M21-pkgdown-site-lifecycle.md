# M21: A site that knows its version, and deploys that leave nothing behind

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the deliverable is the published documentation site users read
- **Branch/PR:** `m021-pkgdown-site-lifecycle`

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
gate and held as a ROADMAP candidate row. Neither lane is measured at its
shipped target. The release-lane preview run measures `clean-exclude: dev`
against an explicit `target-folder`, showing that the pattern protects a `dev`
path under the deploy target. The run also showed the pattern is not anchored
at the target: a `dev` directory nested below the target root survived too, so
`clean-exclude: dev` spares a `dev` path at any depth under the target. How the
action behaves with the branch root as its target is not measured. The dev lane
is likewise measured at
`m021-preview/dev` rather than at the shipped `dev/`, so that its cleaning is
scoped to its target at any depth is inferred from that run. A version dropdown or custom
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
      built tree, not from the event. One step, `Pick the deploy lane`, tests
      whether `docs/dev` exists and writes `lane=dev` or `lane=release` to
      `$GITHUB_OUTPUT`, and both deploy steps — `Deploy development docs 🚀`
      and `Deploy release docs 🚀` — read that output. Each of those two
      steps' `if:` is exactly `github.event_name != 'pull_request' &&
      steps.lane.outputs.lane == '<lane>'`, with `<lane>` the string literal
      `dev` or `release`, and no other operator or operand appears in either
      expression. `pkgdown.yaml` contains no deploy step other than these two.
      The lane step's `[ -d docs/dev ]` test resolves `dev` against a built
      tree containing `docs/dev` and `release` against one without it.
- Shared by AC3 and AC4: each is MEASURED by a push-triggered run of the
  preview workflow's copy of the shipped step named, every key of whose
  `with:` block equals the shipped step's but for `target-folder`, shown by a
  diff of the two extracted step blocks. No probe's path occurs in the built
  site, no probe is named `.nojekyll`, `.git`, `.github` or `.ssh`, every
  directory probe holds exactly one file, and `git diff` between the
  `gh-pages` commits immediately before and after the run reports no path
  outside the target folder changed.
- [ ] AC3: The shipped dev-lane deploy step's cleaning is scoped to its
      `target-folder`. Four items planted under the target beforehand — an
      ordinary file at the target root, a dotfile at the target root, an
      ordinary file in a nested subdirectory, and a directory — are all absent
      from the tree afterwards.
- [ ] AC4: The shipped release-lane deploy step's cleaning spares `dev` under
      its target while removing other stale content, its copy carrying the
      release lane's exact deploy inputs (`clean: true`, `clean-exclude: dev`)
      and differing only by an added `target-folder`. Two items planted under
      the target survive byte-identical, `dev/<file>` and `dev/<sub>/.<file>`,
      while two planted elsewhere under the target — an ordinary file at the
      target root and a directory at depth, neither named `dev` — are absent
      afterwards.
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

- [x] T1: Add `development: mode: auto` to `_pkgdown.yml` (top level, beside
      `template:`). Build twice from a removed `docs/` — once plain, once
      under `PKGDOWN_DEV_MODE=release` — capturing
      `fs::dir_ls("docs", all = TRUE)` each time.
- [x] T2: Rewrite the deploy tail of `.github/workflows/pkgdown.yaml`
      (currently one step at the file's end with `clean: false`): add a lane
      step after "Build site" setting `lane=dev` or `lane=release` from
      `[ -d docs/dev ]` into `$GITHUB_OUTPUT`, then two deploy steps — dev
      (`folder: docs/dev`, `target-folder: dev`, `clean: true`) and release
      (`folder: docs`, `clean: true`, `clean-exclude: dev`) — each keeping the
      existing `github.event_name != 'pull_request'` guard alongside its lane
      condition. Run the lane test locally against both T1 trees. Capture both
      deploy steps' `if:` keys in full, for the review to quote.
- [x] T3: Add `.github/workflows/pkgdown-preview.yaml`, `workflow_dispatch`
      only, copying T2's lane step and each of T2's two deploy steps verbatim
      except `target-folder`, which points under a single preview folder. Give
      the dispatch a `mode` input that sets `PKGDOWN_DEV_MODE` for the build,
      so one dispatch builds the auto-mode tree and the other the release-mode
      tree and the copied lane step decides each run's lane from the tree it
      got. Confirm `.Rbuildignore`'s `^\.github$` covers it (no new entry
      expected).
- [x] T4: For each lane: plant that criterion's items on `gh-pages` under the
      preview folder, record `git rev-parse origin/gh-pages` and the tree,
      dispatch the preview workflow, record the tree after, and produce the
      `git diff` between the two commits. Two runs, evidence for AC3 and AC4.
      Capture each run's lane-step log as cited evidence that the copied step
      resolves `dev` in the auto-mode run and `release` in the release-mode
      run, and record each run's head commit alongside it, so a reader can see
      the deploy steps were unchanged between the two. Also record whether a
      `dev` directory nested below the target root survived run 2, and quote
      what the run showed for it.
- [ ] T5: Write the `NEWS.md` entry. If `tests/spelling.Rout.save` drifts,
      regenerate with `spelling::update_wordlist(confirm = FALSE)` — never by
      hand-editing `inst/WORDLIST` (M20 lesson).
- [ ] T6: Run the verify slot on the branch, and `devtools::check()` on both
      the branch and `main` at the branch point; capture both NOTE sets.
- [ ] T7: Delete `.github/workflows/pkgdown-preview.yaml` and push that
      deletion FIRST, so no later push of the branch can fire a run that
      re-creates the preview folder. Then remove the preview folder from
      `gh-pages`. Confirm both with `git ls-tree -r`.

## Work log

- 2026-10-06: substantive amendment: AC2 rewritten. Two defects in the planned wording. First, "none names a lane" read literally forbids the only condition that can select a lane from the lane step's output, which T2 mandates. Second, "the branch's own pull-request CI run" cannot exist when review verifies criteria, because the PR opens only after the merge approval. The amended AC2 states the two deploy steps' `if:` expressions verbatim, names both steps instead of quantifying over "every deploy step", and binds the lane step's own `[ -d docs/dev ]` decision against a tree with and without `docs/dev`. Two evidence-quotation clauses moved out of the criterion into T2 and T4 as instrument properties. Deliverable unchanged, so no user stop. T3 now also copies the lane step and takes a build-mode dispatch input, so both lane values are observed in real dispatched runs. Coverage unchanged (AC2 to T2).
- 2026-10-06: re-audit: AC2 (full) — returned 8 findings, all applied. Undefined "lane condition" sub-term, "gates deploy-or-not only" self-contradiction, a local run of a step body that writes to `$GITHUB_OUTPUT` and reports nothing locally, two AC1 trees that never coexist, an unenumerated "every deploy step" domain, two instrument-bound evidence-quotation clauses, and no criterion observing the shipped workflow executing at all.
- 2026-10-06: re-audit: AC2 (full) — returned 3 findings on the fixed wording, all applied. "Exactly two terms" had no stated unit of counting, so the `if:` is now given verbatim. The evidence sentence attributed a preview-workflow run to the shipped file and rested on a build step no criterion mandated. That sentence was instrument-bound, so it narrowed to the lane step's own decision and the dispatched-run logs moved to T4. The reader's one loosening note was also applied: AC2 now states that `pkgdown.yaml` holds no third deploy step. Re-entry spent, no further reader for AC2.
- 2026-10-06: T4 done, release lane. Run 37515072961, head 19cf03aa, success. Lane step picked release: `Deploy release docs 🚀` ran and `Deploy development docs 🚀` was skipped. `gh-pages` before a165410, after d97dd60. Both survivors byte-identical by blob hash: `m021-preview/release/dev/KEEP-FILE.txt` at 1d2e405 and `m021-preview/release/dev/sub/.keep-dotfile` at 6dcc03f. Both removal probes absent: `m021-preview/release/ZZ-stale-root.txt` and `m021-preview/release/stale-deep/sub/ZZ-only-file.txt`. `git diff --name-only a165410 d97dd60` lists no path outside `m021-preview/release/`. The `dev` directory nested below the target root, `m021-preview/release/reference/dev/ZZ-nested-dev.txt`, SURVIVED at 90e4a80, so `clean-exclude: dev` matches a `dev` path at any depth under the target rather than only the target's own `dev` child. The Out section was corrected to state that, since it had listed the anchoring question as unmeasured.
- 2026-10-06: T4 done, dev lane. Run 37514709251, head 7ada2b0f, success. Lane step picked dev: `Deploy development docs 🚀` ran and `Deploy release docs 🚀` was skipped. `gh-pages` before f2b7959, after 5aaf9a1. All four probes absent afterwards: `m021-preview/dev/ZZ-stale-root.txt`, the dotfile `m021-preview/dev/.zz-stale-root`, `m021-preview/dev/nested/sub/ZZ-stale-nested.txt`, and `m021-preview/dev/stale-dir/ZZ-only-file.txt`. None of the four paths occurs in what the build writes, checked against the deployed tree of the earlier run. The sentinel `m021-preview/ZZ-OUTSIDE-SENTINEL.txt` survived, and `git diff --name-only f2b7959 5aaf9a1` lists no path outside `m021-preview/dev/`. Between the two runs the preview workflow changed only in one comment line and the `PKGDOWN_DEV_MODE` literal, so the deploy steps were identical.
- 2026-10-06: the amendment put the plan-owned body at 150 lines, one over the cap. Compressed the heaviest plan-owned section, Acceptance criteria, in one pass: the clauses AC3 and AC4 state identically now sit in one shared line above them. No promise changed. Validate green again.
- 2026-10-06: substantive amendment: AC3 and AC4 rewritten, Out extended, T4 and T7 given further steps. The user chose to apply all ten findings of the second audit round at the wording stop. AC3 and AC4 now make the shipped deploy step the subject and the preview run the procedure, name the `with:`-block diff that enumerates the key set each claims equal, and constrain the planted probes: four probes in AC3 crossing the root/nested and ordinary/hidden axes, two removal probes in AC4, no probe named `.nojekyll` or colliding with a built-site path, and every directory probe holding exactly one file. AC4 drops the nested `dev` probe, which could fail while the shipped release lane is correct, and T4 now records it instead. Out declares that neither lane is measured at its shipped target and states the release-lane inference in a form the nested-`dev` result cannot contradict. T7 pushes the workflow deletion before clearing the gh-pages folder. Run 1 must be redone, because the probe set changed. Deliverable unchanged, so no further stop.
- 2026-10-06: run 1 measured the dev lane. Preview run 37512555826, push event, head 378c7927, conclusion success. `gh-pages` before 490baef, after 6819e55. All three planted probes absent afterwards: the dotfile `m021-preview/dev/.stale-root`, the nested ordinary file `m021-preview/dev/nested/sub/STALE-NESTED.txt`, and the stale directory's only file `m021-preview/dev/stale-dir/STALE-DIR-FILE.txt`. The sentinel `m021-preview/OUTSIDE-SENTINEL.txt`, planted outside the target, is present and unchanged. `git diff --name-only 490baef 6819e55` lists no path outside `m021-preview/dev/`. The deployed tree contains `m021-preview/dev/.nojekyll`, which the build writes, so a probe named `.nojekyll` would have read as survival.
- 2026-10-06: re-audit: AC3 (full) — returned 5 findings shared with AC4. Trigger wording too loose, probe-set axes, the push trigger's missing path filter, T7 ordering, and the hidden-file axis. All applied, which spent the one re-entry.
- 2026-10-06: re-audit: AC4 (full) — returned the same 5 findings. Additionally: the shipped release step has no `target-folder` key at all, so "differing only in `target-folder`" was false as written, and two clauses mandating what the review states are instrument-bound. All applied.
- 2026-10-06: re-audit: AC3 (full) — the re-entry returned 10 further findings. A `.nojekyll` probe would read as survival, probes may collide with built-site paths, "a stale directory" is not plantable as stated because git tracks no empty directory, three probes cannot cross three binary axes, the run rather than the shipped step is the grammatical subject, and "differing only in `target-folder`" quantifies over a key set no named procedure enumerates. Second re-entry on this criterion, so no further reader is spawned and the wording question goes to the user.
- 2026-10-06: re-audit: AC4 (full) — the re-entry returned the same 10 findings. Additionally: the removal half carries one probe varied on no axis, the nested `dev` probe can fail while the shipped release lane is correct, and the new Out sentence states an inference that probe's survival would contradict. Second re-entry on this criterion, so the wording question goes to the user.
- 2026-10-06: the plan's dispatch route is unavailable, measured rather than inferred. `gh workflow run pkgdown-preview.yaml --ref m021-pkgdown-site-lifecycle -f mode=auto` answered `HTTP 404: workflow pkgdown-preview.yaml not found on the default branch`. Putting the throwaway workflow on the default branch is what the git model forbids, so the preview workflow now triggers on a push of this branch narrowed to its own path, and `PKGDOWN_DEV_MODE` is a literal edited between the two runs. The measurement is the same in substance: two real runs of verbatim copies of the shipped deploy steps against the real `gh-pages`. The path filter means no other push of this branch fires a run.
- 2026-10-06: T3 done. `pkgdown-preview.yaml` added, `workflow_dispatch` only, with a `mode` choice input (`auto` or `release`) feeding `PKGDOWN_DEV_MODE` on the build step. `diff` of the extracted step blocks shows the lane step byte-identical to the shipped one, the dev deploy step differing only in `target-folder` (`m021-preview/dev` for `dev`), and the release deploy step differing only by the added `target-folder: m021-preview/release`. Both files parse under `yaml::read_yaml()`, and the shipped file carries exactly two `if:` keys, one per deploy step. `.Rbuildignore`'s `^\.github$` covers the new file, so no new entry. `devtools::test()`: FAIL 0, WARN 0, SKIP 8, PASS 1154.
- 2026-10-06: T2 done. `pkgdown.yaml`'s single `clean: false` deploy step replaced by a `Pick the deploy lane` step writing `lane=dev` or `lane=release` to `$GITHUB_OUTPUT`, then two deploy steps. Both `if:` keys read in full from the file: `github.event_name != 'pull_request' && steps.lane.outputs.lane == 'dev'` at line 79 and the same with `'release'` at line 90. `grep -n 'name: Deploy'` finds those two deploy steps and no third. The lane test reports `dev` against the auto-mode tree and `release` against the release-mode tree. `devtools::test()`: FAIL 0, WARN 0, SKIP 8, PASS 1154.
- 2026-10-06: T1 done. `development: mode: auto` added to `_pkgdown.yml` beside `template:`. From a removed `docs/`, the plain build left `fs::dir_ls("docs", all = TRUE)` reporting exactly `docs/dev`; the same build under `PKGDOWN_DEV_MODE=release` wrote `docs/index.html` and no `docs/dev`. `devtools::test()`: FAIL 0, WARN 0, SKIP 8, PASS 1154. `install = FALSE` needs openac in the library, so the branch was installed once with `devtools::install(quick = TRUE)` before building.
- 2026-10-06: status set in-progress, branch `m021-pkgdown-site-lifecycle` cut from the pushed `main` (already up to date, nothing unpushed). Tree was clean at the cut.
- 2026-09-06: created by /milestone-plan.
- 2026-09-06: criteria audit ran in FULL mode (user-facing tier); returned 10 findings over 6 draft criteria. Nine applied before writing: AC1 gained `all = TRUE` and a removed-`docs/` precondition; AC2's ban narrowed to lane selection and given an enumerating procedure (quote every deploy-step `if:`); AC3 and AC4 each given three planted probes varying form and depth; AC4 gained AC3's outside-the-target diff clause; AC5's two false root claims rescoped; AC6's "any NOTE justified" replaced by the `main`-at-branch-point NOTE set. The tenth — the release lane's real target is the `gh-pages` root, which no safe measurement reaches — is carried as a stated limitation in AC4 and in Out. AC7 was added afterwards to bind the cleanup, and went back through the audit's questions.
- 2026-09-06: plan gate chose `development: mode: auto` over `mode: unreleased` and over a clean-only fix because `unreleased` forces the banner regardless of version and still lets the root flip between release and dev content, and clean-only locks in the overwrite; falsified by a pkgdown release whose `auto` resolution puts a dev version at the root.
- 2026-09-06: plan gate chose to leave the stale `gh-pages` root over hand-committing a redirect and over rebuilding it from the `v0.1.0` tag, because a redirect makes part of the deliverable an out-of-band commit no CI reproduces and the tag's tree has no `_pkgdown.yml`; falsified by evidence that the first release-lane deploy does not replace the root wholesale.
- 2026-09-06: plan gate chose real dispatched preview runs on `gh-pages` over reading the deploy action's documentation, because LESSONS records two days lost to inferred tool behavior (M13's `cmd2` claim, M16's HTTP-200 dead links); falsified by the preview target proving to differ from the shipped target on the cleaning axis.

## Decisions

## Review
