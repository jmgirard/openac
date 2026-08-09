# M15: What Windows actually does to a path the shell can eat

**Status:** done (2026-08-08, PR #15 https://github.com/jmgirard/openac/pull/15)

**Goal:** Settle by measurement on a real Windows host what reaches an external
tool when a filename carries a `cmd.exe` metacharacter, and make openac's
Windows quoting — and the claims this repo makes about it — match the answer.

**Outcome:** No behavior change: the `%TEMP%` gap M13 recorded was inferred and
is false. MEASURED on Windows 11 build 26100 / R 4.6.1, all eight names of the
new `hostile_names()` table round-tripped through real ffmpeg and ffprobe
intact — `system2()` puts no `cmd.exe` between openac and the tool, so `cmd2`
escaping would guard a shell that is not there. `test-real-tools.R` gained one
`test_that()` per name (its first Windows run ever); `quote_type()` and
`quote_tokens()` split the rule out of `run_tool()` so the Windows style is
assertable from any host. The falsified claim was corrected in NEWS.md, DESIGN,
LESSONS and all four `@param arg` blocks. `.gitattributes` pins LF.

**Decisions:** none; D-017 unchanged, now pinned on `quote_tokens()` too.

**Review:** three lenses, 22 findings; scorer actioned F1 (85, NEWS still
shipped the falsified claim) and F5 (80, corrected paragraphs still said "the
shell"), both fixed. Five below-bar fixed as false-as-written; F9 rejected;
F8/B4 (78, CRLF churn) closed by the maintainer's LF call.
OpenFace-writes-no-CSV → candidate row. No lessons retired; two captured.
