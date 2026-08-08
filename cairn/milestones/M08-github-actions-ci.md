# M08: GitHub Actions CI — R CMD check across platforms

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3, GP7

- **Branch/PR:** `m08-github-actions-ci` · https://github.com/jmgirard/openac/pull/7

## Goal

Give the repo a working `R CMD check` workflow on Windows, macOS and Linux, so
the review gate's never-merge-red-CI rule has something to read.

## Scope

**In:** `.github/workflows/R-CMD-check.yaml` from
`usethis::use_github_action("check-standard")`, configured to skip the
`Remotes:`-backed `audio.whisper` Suggest; the `^\.github$` `.Rbuildignore`
entry; watching the first full run on this milestone's own PR and fixing what
it surfaces within a bounded remit.

**Out:** the `test-coverage` / Codecov workflow → candidate row (it needs a
`CODECOV_TOKEN` repository secret only the maintainer can add, and a job that
cannot authenticate blocks every later merge). Branch-protection and
required-status-check settings → candidate row (GitHub repo settings, not
files). Removing `Remotes:` for CRAN → the existing CRAN-readiness candidate.
Platform breakage needing design work → its own follow-on milestone, per AC2.

## Acceptance criteria

- [x] AC1 `.github/workflows/R-CMD-check.yaml` exists and, read as text: its
      `on:` block triggers on `pull_request` and on `push` to `main` (the
      branch `git symbolic-ref --short refs/remotes/origin/HEAD` reports); its
      matrix declares at least one Windows, one macOS and one Linux job; and
      its check step's effective `error-on` is `"warning"`, either written
      explicitly or read from the `action.yml` of the pinned
      `r-lib/actions/check-r-package` version, with whichever applies recorded.
- [x] AC2 Every job that workflow declares concludes `success` on this
      milestone's own PR. The domain is enumerated by the workflow run's own
      job list on its GitHub run page, and each job's status is read from
      `gh pr checks <PR>`; both are recorded in the Review section. A run
      registering no jobs, or a job skipped, cancelled or still queued, does
      not satisfy this. Any entry dropped from usethis's default matrix is
      named in the Review section with the failure that caused the drop, plus
      its ROADMAP candidate row and follow-on milestone.
- [x] AC3 `.Rbuildignore` contains the literal line `^\.github$`, and
      `devtools::check()` on the branch reports no "non-standard
      file/directory found at top level" NOTE naming `.github`.
- [x] AC4 `audio.whisper` is absent from the installed library of every job
      AC2 enumerates, while `testthat`, `withr`, `knitr`, `rmarkdown` and
      `spelling` are present — evidenced by each job's dependency-listing step
      and check log, cited by URL. No job installs any package from
      `Remotes:`.
- [x] AC5 Every file changed under `R/` on this branch — the domain enumerated
      by `git diff --name-only $(git merge-base main HEAD)..HEAD -- R/` —
      exists to turn a named CI job green, and each change is recorded in the
      work log with its observed failing-before evidence: a testthat test that
      fails without the change where testthat can observe the defect,
      otherwise the `R CMD check` output that fails without it. An empty diff
      satisfies this vacuously, stated as such in the Review section.
- [x] AC6 On the branch, `devtools::document()` produces no diff and
      `devtools::test()` passes.

## Coverage

- AC1 → T1, T2
- AC2 → T4, T5, T6
- AC3 → T1, T3
- AC4 → T2, T6
- AC5 → T5
- AC6 → T3

## Tasks

- [x] T1 Run `usethis::use_github_action("check-standard")`; confirm the
      `^\.github$` entry landed in `.Rbuildignore` and add it if not. Also
      re-knit `README.Rmd` — usethis adds a badge (discovered T1).
- [x] T2 Configure the workflow to skip `Remotes:`-backed Suggests: request
      hard dependencies plus `rcmdcheck`, `testthat`, `withr`, `knitr`,
      `rmarkdown`, `spelling` as extras, and set `_R_CHECK_FORCE_SUGGESTS_:
      false` on the check step. Verify the `on:` block and the effective
      `error-on` per AC1.
- [x] T3 Run `document()`, `test()`, `check()` locally on the branch; record
      the local NOTE set and confirm no `.github` top-level NOTE.
- [x] T4 Push the branch, open the PR, and watch the first full run with
      `gh pr checks <PR> --watch`; record the run URL and per-job outcome.
- [x] T5 Triage failures. Fix what is solvable inside this milestone; for
      anything needing design work, drop that matrix entry, add a ROADMAP
      candidate row, and plan the follow-on milestone. Each `R/` change
      carries its AC5 failing-before evidence.
- [x] T6 Re-run to green; record the final `gh pr checks` output and each
      job's package listing showing `audio.whisper` absent.

## Work log

- 2026-08-07: created by /milestone-plan, promoting the CI candidate row added at M06 review 2.
- 2026-08-07: criteria audit [O] returned five findings plus a coverage gap; fixed pre-gate — AC1's "generated by usethis" clause was unsatisfiable alongside its `error-on` clause (usethis fetches the upstream template, which carries no `error-on` line), AC2's `gh pr checks` enumerated check runs rather than declared jobs so a workflow that never ran passed vacuously, and AC5's two-dot diff spanned the default branch's own commits; routed to the gate — AC4's audio.whisper state was unreachable under the unmodified workflow, and no criterion covered the profile's second workflow.
- 2026-08-07: plan gate chose skipping `audio.whisper` on CI over installing it on every job (and over installing it on one Linux job) because nothing in the check surface reaches it — no test loads it, every `@examples` block is `\dontrun{}`, both vignettes set `eval = FALSE`, and `man/` carries no `\link[audio.whisper]{}` — while installing it compiles whisper.cpp from an unpinned GitHub source on every runner; falsified by a check failure that only appears when the package is present.
- 2026-08-07: plan gate chose deferring the `test-coverage` workflow over shipping it now or shipping it non-blocking, because Codecov requires a repository secret only the maintainer can add and an unauthenticated job goes red under a rule that blocks every later merge; falsified by the token existing before this milestone's PR is opened.
- 2026-08-07: plan gate chose bounded fallout repair over fixing every platform here or merging red under a waiver, because macOS and Linux have never been exercised and the repair size is unknown at plan time; falsified by the first full run coming back green or with only mechanical failures.

- 2026-08-07: T1 — `use_github_action("check-standard")` wrote `.github/workflows/R-CMD-check.yaml`, added `^\.github$` to `.Rbuildignore`, and added an R-CMD-check badge to `README.Rmd`; re-knitted with `build_readme()`, a sub-task the plan did not anticipate (minor amendment, T1 wording extended).
- 2026-08-07: T2 — pinned `dependencies: '"hard"'` plus named test-only extras and set `_R_CHECK_FORCE_SUGGESTS_: false` at job level; added a "Confirm audio.whisper is absent" step that fails the job on `requireNamespace()` succeeding and prints the installed-package listing AC4 cites.
- 2026-08-07: T2 — read `r-lib/actions/check-r-package@v2`'s `action.yaml`: `error-on` defaults to `'"warning"'` (AC1 satisfied by the default, no explicit line written), and the action already sets `_R_CHECK_FORCE_SUGGESTS_=false` when unset — the explicit job-level setting is redundant with that behavior and kept to make the intent legible rather than implicit.
- 2026-08-07: T3 — on the branch: `document()` no diff, `test()` 252 pass / 0 fail, `check()` 0 errors 0 warnings 1 NOTE. `checking top-level files ... OK` confirms no `.github` NOTE (AC3). The single NOTE is `checking tests` — the pre-existing `spelling.R` output diff, not introduced here.
- 2026-08-07: T3 — the README badge added one new spelling hit (`CMD`, `README.md:8`), found by grepping the captured check output rather than by eye per M06's lesson; added `CMD` to `inst/WORDLIST`.

- 2026-08-07: T4 — PR #7, run 31232602357. macOS release, Ubuntu devel/release/oldrel-1 all pass; windows-latest (release) fails with `Status: 1 ERROR`, `[ FAIL 2 | WARN 1 | SKIP 0 | PASS 250 ]`. The reverse of DESIGN's Windows-biased-testing expectation: it is Windows that breaks, and the two Ubuntu/macOS-only platforms are clean.
- 2026-08-07: T5 — both Windows failures are one harness defect, not package code. `fake_sys_which` gated its fallback on `file.access(n, 1L) == 0L`; Windows returns -1 there for the extensionless `Sys.chmod("0755")` fixture binaries, so `find_program()` returned NULL (test-programs-resolve.R:35) and `set_program()`'s `stopifnot(Sys.which(location) != "")` aborted at programs_set.R:15 (test-programs-resolve.R:60). Failure identity confirmed from the job log's named assertions and backtraces, not from the bare red job.
- 2026-08-07: T5 — extracted `fake_is_executable()` in `helper-openac.R`, degrading executability to existence on Windows and keeping `file.access` on Unix. Verified no test depends on an existing-but-non-executable file failing to resolve — the stale-config test uses a nonexistent path, so it still fails `file.exists()`. Local `test()` after the fix: 252 pass, 0 fail.
- 2026-08-07: corrected the M06 `Sys.which()` lesson in `LESSONS.md` in place and marked it `(M06, corrected M08)` — it was true on Unix and silently wrong on Windows, which is what cost this milestone a CI round. AC5's `R/` domain stays empty: the defect was in `tests/`, and no package code changed.

- 2026-08-07: T6 — re-run 31233107112 all five declared jobs `success`; `gh run view --json jobs` enumerates exactly the five the matrix declares, so no entry was dropped and AC2's matrix-drop clause has nothing to record. The `audio.whisper` guard step passed on every job.
- 2026-08-07: T6 — AC4 verified against the `installed.packages()` listing itself, not a grep over the step block: the first pass matched `audio.whisper` in the step's own name and echoed script, so the listing lines were isolated (19 lines) and re-checked — zero `audio.whisper` occurrences, all five named extras present.
- 2026-08-07: completion — local `check()` after the harness fix: 0 errors, 0 warnings, 1 NOTE (unchanged pre-existing `spelling.R` output diff); `document()` no diff; `test()` 252 pass. `cairn_validate` all checks passed. Status → review.
- 2026-08-07: review — six criteria verified with fresh evidence; consistency gate clean; three lenses returned 15 findings, none scoring 80 or above, so the actioned list is empty and no return floor was met. F6/F7 absorbed into the existing harness-hardening candidate row.

## Decisions

## Review

Reviewed 2026-08-07 on `m08-github-actions-ci` @ `6f58748`, PR #7. `main` in
sync with origin and already contained by the branch — no merge needed.

### Acceptance-criterion evidence (fresh)

- **AC1** — `R-CMD-check.yaml` read as text: `on:` triggers `pull_request` and
  `push` to `[main, master]`; `git symbolic-ref refs/remotes/origin/HEAD`
  reports `origin/main`, so the default branch is covered. Matrix declares
  macos-latest, windows-latest and ubuntu-latest ×3 — Windows, macOS and Linux
  all present. No `error-on` line in the workflow (grep count 0); the pinned
  `r-lib/actions/check-r-package@v2` `action.yaml` was re-fetched at review and
  its `error-on` default reads `'"warning"'`, so the effective value is
  inherited, not written.
- **AC2** — run 31233326924 on head `6f58748`, conclusion `success`.
  `gh run view --json jobs` enumerates exactly five jobs, matching the five
  matrix entries; all five `success`. `gh pr checks 7` reports all five `pass`.
  No matrix entry was dropped, so the drop-accounting clause has nothing to
  record.
- **AC3** — `.Rbuildignore:9` holds the literal `^\.github$`. Fresh
  `devtools::check()`: `checking top-level files ... OK`, and zero occurrences
  of "non-standard file/directory" anywhere in the output.
- **AC4** — verified across all five jobs AC2 enumerates, not a sample. The
  guard step passed on each; each job's `installed.packages()` listing (19
  lines; 20 on Windows) contains zero `audio.whisper` occurrences, and
  `testthat`, `withr`, `knitr`, `rmarkdown`, `spelling` are present on every
  job. `audio.whisper` is the sole `Remotes:` entry, so no job installed from
  `Remotes:`. A first pass at this evidence ran `gh` from outside the repo and
  returned all-zero counts; the fetch, not the evidence, was empty — re-run
  from the repo root before anything was concluded.
- **AC5** — `git diff --name-only $(git merge-base main HEAD)..HEAD -- R/`
  returns 0 files. No package code changed, so the criterion is satisfied
  vacuously, as its own wording provides for.
- **AC6** — fresh `devtools::document()` leaves `man/` and `NAMESPACE` clean
  (0 porcelain entries); `devtools::test()` 252 pass, 0 fail, 0 skip.

### Consistency gate

`cairn_validate` exit 0 — 15 checks PASS, 8 advisories OK. `DESIGN.md` is
unchanged on the branch, so no `cairn_impact` run is owed. Profile
`consistency-gate` slot: `document()` no diff · README.md re-knitted and in
sync · no pkgdown site (N/A) · `.github` carries its `.Rbuildignore` entry and
`checking top-level files` is OK · `devtools::check()` 0 errors, 0 warnings, 1
NOTE (the pre-existing `spelling.R` output diff, unchanged from `main`).
No `NEWS.md` entry: the milestone changes no user-visible package behavior.

### Independent review — three lenses, then scorer

[O] diff-bug returned 14 findings; [S] blame-history returned 1 plus a
no-regression verdict on the M06 intent behind the changed predicate; [S]
prior-review reported no regression of any M06 review-2 point (its GitHub
inline-comment probe came back empty, so the archive was the only surface).
15 findings scored by a fresh [S] scorer holding the diff and the plan.

**Nothing scored ≥80, so the actioned list is empty and no finding met the
return floor.** All 15 logged, highest first:

- F6 (65) Windows branch of `fake_is_executable()` resolves any existing path,
  where real Windows `Sys.which()` resolves by executable extension — a
  fidelity gap in new code.
- F7 (65) neither branch of `fake_is_executable()` is directly tested;
  `test-helper-boundary.R` exists for exactly that purpose.
- F4 (55) `requireNamespace()` tests loadability, not AC4's literal installed-library domain.
- F13 (45) ROADMAP hygiene stamp reads "M08 planned" while the row reads `review`.
- F2 (35) `_R_CHECK_FORCE_SUGGESTS_` sits at job scope, wider than T2's wording.
- F3 (35) the guard step asserts absence but only prints the extras' presence.
- F1 (30) excluding `audio.whisper` skips R CMD check's `::` symbol resolution — the planned trade-off.
- F10 (30) CI's NOTE set (2, incl. the suggested-package NOTE) not recorded against the local 1.
- F14 (30) the in-place LESSONS correction carries no D-entry.
- F11 (20) `@v2` is a moving tag, so AC1's inherited `error-on` is not pinned.
- F8 (15) helper spliced into another function's comment block.
- F9 (15) neighbouring comment now stale — unmodified line.
- B1 (5) LESSONS header cites plugin-level decision ids — pre-existing on `main`.
- F12 (2) stale: written before this Review section existed.
- F5 (2) premise false — upstream `check-standard.yaml` carries no
  `use-public-rspm`; the workflow matches the template exactly.

F6 and F7 were absorbed into the existing "Harden the system2 test harness"
candidate row (search-first: that row already owns `helper-openac.R` hardening
from M06 review 2) rather than added as a duplicate row.
