# Lessons

Durable repo lessons — build quirks, testing tricks, gotchas worth
remembering next time — captured at milestone end and surfaced at plan time.
Not status, not decisions: a lesson is a reusable "how this repo actually
behaves" note. Cross-cutting *choices* still go to `DECISIONS.md`.

One line per lesson: `- YYYY-MM-DD (M<NN>): <lesson>`. One cap
(tracking-rules weight-caps): 50 lines, met by retiring or pruning entries.
Current knowledge: a lesson proven false is corrected in place (D-045).
Lessons also *leave*: one retires when a test fails on the mistake it warns
about, when another file's slot owns its content, or when a matured family
graduates whole into a doctrine module; pruning the stalest is the last resort
rather than the first (D-051, D-055).

- 2026-08-07 (M06): `tools::file_path_as_absolute()` resolves the macOS `/var` → `/private/var` symlink, so a test comparing a returned path against a bare `tempdir()` path fails — canonicalize the expectation, not the code.
- 2026-08-07 (M06, corrected M08): `Sys.which()` returns `""` for a file that exists but is not executable, so a fake binary in a test fixture must be created mode 0755 — but that holds on Unix only: Windows has no execute bit, `file.access(mode = 1)` returns -1 there for an extensionless file whatever `Sys.chmod()` did, so a fixture's executability predicate must degrade to plain existence on Windows.
- 2026-08-07 (M06): testthat runs test files in sorted filename order, so a test that counts what other files did must sort last (this repo uses a `test-zzz-` prefix) and must skip rather than fail when run in isolation.
- 2026-08-07 (M08): `usethis::use_github_action()` also adds a badge to README.Rmd, so the run must be followed by `devtools::build_readme()` — and the badge text adds a new spelling hit (`CMD`) that belongs in `inst/WORDLIST`.
- 2026-08-07 (M08): `gh` resolves the target repo from the working directory, so a loop that `cd`s to a scratch dir returns empty output for every job — which reads as "evidence absent" rather than "fetch failed". Keep `gh` calls in the repo root and write to absolute paths.
- 2026-08-07 (M08): `r-lib/actions/check-r-package@v2` already defaults `error-on` to `"warning"` and sets `_R_CHECK_FORCE_SUGGESTS_=false` itself, so the stock `check-standard` workflow needs neither line — read the pinned action's `action.yaml` (not `action.yml`) before adding one.
- 2026-08-07 (M06): the spelling NOTE's word list differs between `spelling::spell_check_package()` on the source and the tarball's `spelling.Rout` diff — compare branch against the default branch with one method rather than matching a remembered count, and check new roxygen/NEWS prose before committing.
- 2026-08-07 (M07): `testthat::local_mocked_bindings(.package = "stats")` does not intercept a generic openac *imports* — the package's imports environment resolves the binding first, so dispatch reaches the real generic and fails with "no applicable method". Mock in openac's own namespace (omit `.package`) to shadow an imported function.
- 2026-08-07 (M07): `list.files(recursive = FALSE)` returns directories alongside files (the recursive form omits them), and `file.exists()` is TRUE for a directory — so any extension-matched listing that feeds a per-file operation must filter with `!dir.exists()`, or a directory named `clips.mp4` is handed to the tool as input.
