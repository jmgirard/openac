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
- 2026-08-07 (M09, retiring the M06/M08 executability lesson): `Sys.which()` was MEASURED on GitHub runners rather than reasoned about, twice having been guessed wrong. Unix resolves an existing file iff `file.access(path, 1L) == 0`, extension irrelevant. Windows resolves it iff it carries any extension — `.txt` resolved at 0755 while `file.access()` said -1, so mode plays no part there — or, extensionless, iff a `.com`/`.exe`/`.bat`/`.cmd` sibling exists, and it returns that sibling's path. A `.txt` sibling does not resolve although a `.txt` file named directly does: the two rules are different, and reading one off the other is how the sibling set got written as `.exe` alone. So a Windows test fixture must carry an extension, and `test-helper-boundary.R` now fails on the mistake the old lesson only warned about.
- 2026-08-07 (M06): testthat runs test files in sorted filename order, so a test that counts what other files did must sort last (this repo uses a `test-zzz-` prefix) and must skip rather than fail when run in isolation.
- 2026-08-07 (M08): `usethis::use_github_action()` also adds a badge to README.Rmd, so the run must be followed by `devtools::build_readme()` — and the badge text adds a new spelling hit (`CMD`) that belongs in `inst/WORDLIST`.
- 2026-08-07 (M08): `gh` resolves the target repo from the working directory, so a loop that `cd`s to a scratch dir returns empty output for every job — which reads as "evidence absent" rather than "fetch failed". Keep `gh` calls in the repo root and write to absolute paths.
- 2026-08-07 (M08): `r-lib/actions/check-r-package@v2` already defaults `error-on` to `"warning"` and sets `_R_CHECK_FORCE_SUGGESTS_=false` itself, so the stock `check-standard` workflow needs neither line — read the pinned action's `action.yaml` (not `action.yml`) before adding one.
- 2026-08-07 (M06): the spelling NOTE's word list differs between `spelling::spell_check_package()` on the source and the tarball's `spelling.Rout` diff — compare branch against the default branch with one method rather than matching a remembered count, and check new roxygen/NEWS prose before committing.
- 2026-08-07 (M07): `testthat::local_mocked_bindings(.package = "stats")` does not intercept a generic openac *imports* — the package's imports environment resolves the binding first, so dispatch reaches the real generic and fails with "no applicable method". Mock in openac's own namespace (omit `.package`) to shadow an imported function.
- 2026-08-07 (M07): `list.files(recursive = FALSE)` returns directories alongside files (the recursive form omits them), and `file.exists()` is TRUE for a directory — so any extension-matched listing that feeds a per-file operation must filter with `!dir.exists()`, or a directory named `clips.mp4` is handed to the tool as input.
- 2026-08-07 (M09): `normalizePath(p, mustWork = FALSE)` returns a path it cannot resolve *unchanged*, so `identical(p, normalizePath(p, mustWork = FALSE))` is not an absoluteness test — it is TRUE for every relative path that does not exist. Match the three absolute forms explicitly (POSIX, UNC, Windows drive).
- 2026-08-07 (M09 review): a probe that materializes every fixture in one directory cannot say which one answered. M09's first `Sys.which()` probe created `tool` and `tool.exe` side by side, so the extensionless file resolved every query, the sibling rule was never observed, and the criterion written from it was inferred — shipping a branch an earlier guard made unreachable, with the wrong sibling set. Give each case its own directory holding exactly one file.
- 2026-08-08 (M09 review): an R default argument is a promise forced ONCE, at first use, and cached — so a factory whose default reads mutable global state (`os = Sys.info()[["sysname"]]`) pins that state at the returned closure's first call, not at each call. A test that fakes the environment afterwards is then silently ignored. Default such a parameter to `NULL` and resolve it inside the body on every call.
