# M01: openSMILE tidy reader (`os_read`) — done 2026-07-11

**Outcome:** Added exported `os_read(file)` — parses openSMILE CSV output
(aggregate/functionals or per-frame LLD) into a wide tidy tibble. Auto-detects
`;` vs `,` delimiter, preserves non-syntactic feature names
(`check.names = FALSE`), `cli::cli_abort()` on bad input. First member of the
`<tool>_read` reader family. 36 test assertions (hand-built agg/LLD oracles,
delimiter parity, apostrophe-name regression, NaN/Inf/NA, header-only, error
branches). Started `NEWS.md`. PR #1 (squash `d63ca53`).

**Key decisions:** D-004 (`<tool>_read` naming), D-005 (`tibble` → Imports).

**Amendment:** "check() clean" narrowed to "`os_read` adds zero new
errors/warnings vs baseline" — full check-clean blocked by pre-existing
baseline debt, tracked as the "green up R CMD check" ROADMAP candidate.

**Review:** independent Opus review fixed a real bug (single-quote in the
read.csv quote set silently dropped rows for apostrophe-containing instance
names) + added edge-case tests. Follow-up candidate: validate fixtures
against a real openSMILE run (F5).

**Note:** merge exposed that `origin/main` was stale at `2947044` — all prior
cairn history was local-only, so the squash collapsed it into `d63ca53`.
Local main reset to origin; granular history preserved at tag
`pre-squash-local-main`. Lesson: push `main` after tracking commits.
