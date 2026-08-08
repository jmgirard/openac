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
- 2026-08-07 (M06): `Sys.which()` returns `""` for a file that exists but is not executable, so a fake binary in a test fixture must be created mode 0755 or resolution silently fails.
- 2026-08-07 (M06): testthat runs test files in sorted filename order, so a test that counts what other files did must sort last (this repo uses a `test-zzz-` prefix) and must skip rather than fail when run in isolation.
- 2026-08-07 (M06): the spelling NOTE's word list differs between `spelling::spell_check_package()` on the source and the tarball's `spelling.Rout` diff — compare branch against the default branch with one method rather than matching a remembered count, and check new roxygen/NEWS prose before committing.
