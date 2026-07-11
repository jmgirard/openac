# M02: OpenFace tidy reader (`of_read`) — done 2026-07-11

**Outcome:** Added exported `of_read(file)` — parses an OpenFace output CSV
into a wide tidy tibble with one row per detected face per frame (OpenFace is
multi-face). Metadata columns first (`frame`, `face_id`, `timestamp`,
`confidence`, `success`), then all feature blocks passed through as-is. Trims
OpenFace's space-padded column headers (`check.names = FALSE` + `trimws`);
`cli::cli_abort()` on non-string/missing/empty input. Second member of the
`<tool>_read` reader family. 31 test assertions (hand-built 3-frame fixture,
multi-face, header-only 0-row, blank→NA, whitespace trim, AU + non-AU value
oracles, error branches). NEWS entry. PR #2 (squash `4d6b6bb`).

**Key decisions:** inherits D-004 (`<tool>_read` naming) and D-005 (`tibble`
Import) from M01; no new dependency.

**Amendment:** same as M01 — "check() clean" narrowed to "`of_read` adds zero
new errors/warnings vs baseline" (pre-existing debt → cleanup candidate).
Review also corrected the "garbage input" criterion to "empty" (non-empty
garbage is not detected) and fixed a false "one row per frame" doc claim.

**Review:** independent Opus review found no blockers; its findings
(multi-face doc, mandated edge-case tests, non-AU value oracles) were all
fixed before merge (tests 20 → 31). No follow-ups spawned.
