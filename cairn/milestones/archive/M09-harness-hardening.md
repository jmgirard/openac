# M09: Test-harness hardening — fake fidelity at the tool boundary

**Status:** done (2026-08-08, PR #10 https://github.com/jmgirard/openac/pull/10)

**Goal:** Close the harness-fidelity findings the M06, M07 and M08 reviews logged, so the
boundary harness's fakes behave like the functions they stand in for.

**Outcome:** One executability rule, `fake_sys_which_path()`, with `fake_is_executable()` a one-line
view over it and `local_fake_downloads()`'s divergent copy deleted. It was MEASURED on GitHub runners
(R 4.6.1): Windows resolves any-extension files, and an extensionless path through a
`.com`/`.exe`/`.bat`/`.cmd` sibling, RETURNING that sibling's path; Unix reads the mode bit. Fixtures
carry the SIMULATED platform's extension (`fake_program_file()`, stripped by `fake_program_name()`).
Also `is_absolute_path()` refusing a non-absolute command inside the recorder; `boundary_argv()`
preserving argument boundaries; `local_fake_tools()` owning both rappdirs redirects, enumerated from
the namespace; an alias-class lock over `openac_name_of()`; `fake_sys_which()` reading `os` per call.

**Decisions:** D-013 (completeness observed and declared, never inferred — RR02) and D-014
(annotating it: its consequences specify what M10 implements, not this tree). Rejected in-milestone:
`identical(p, normalizePath(p, mustWork = FALSE))` as an absoluteness test, which passes every
non-existent relative path. `test-zzz-command-contract.R` reverted to base; the gate is M10's.

**Review:** Three returns (two defect, one re-cut) then PASSED on the third pass: 25 findings, top
score 55, none actioned. Retired the M06/M08 executability lesson — `test-helper-boundary.R` now
fails on the mistake it warned about — and added three: the measured rule, the `normalizePath` trap,
and one-directory-per-probe-case.
