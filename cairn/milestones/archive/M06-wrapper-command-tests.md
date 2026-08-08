# M06: Wrapper testing contract — system2-boundary command tests

**Status:** done (2026-08-07, PR #6 https://github.com/jmgirard/openac/pull/6)

**Goal:** Give the binary-dependent wrappers their first tests by mocking the
`system2` boundary, so every command openac constructs is asserted with no tool installed.

**Outcome:** `helper-openac.R` mocks `base::system2`/`base::Sys.which` in the calling
frame, recording ordered `(command, args)` plus the outermost openac frame from an
erroring fake-result queue; command tests pin every wrapper the closure enforces.
`test-zzz-command-contract.R` computes that closure by symbol occurrence (27 members,
7 deferred to M07) and fails naming any enforced member with no command test. The
enforced set is the artifact's own, not a list kept here. Fixes: `find_program()` warns and
returns `NULL` rather than erroring and resolves a recorded bare program name; the
passthroughs abort via `require_program()` instead of letting `system2(NULL, args)`
shell-execute the argument string.

**Decisions:** D-010 (mock `system2`, computed coverage gate), D-011 (`withr` to
Suggests), D-012 (testthat floor 3.2.0). Milestone-local: a missing tool aborts the
low-level wrappers.

**Review:** Two rounds. Review 1 returned it — AC2, AC3, AC4 and the changelog gate
failed; T9–T15 closed them. Review 2: three lenses plus scorer, 24 findings, one at
threshold (R1, 85 — NEWS misstated pre-release behavior) fixed at the gate, 23 logged.
