# Decisions

_Append-only. Never renumber; supersede with a new entry. D-entries record
choices with rationale — including genuine rejections. They never record
deferrals ("not now" is a ROADMAP fact, not a decision)._

### D-001 (2026-07-11): Adopt cairn for project tracking

**Context:** openac had no structured project tracking; state lived only in
git history and code.
**Decision:** Adopt the cairn tracking system — `project/` markdown owns all
project state (DESIGN / ROADMAP / milestones / DECISIONS).
**Consequences:** Future work flows through `/milestone-*` skills; status
lives in ROADMAP.md, not CLAUDE.md or memory.

### D-002 (2026-07-11): Waive deprecation ceremony pre-1.0

**Context:** cairn's guardrails require a lifecycle deprecation cycle for
breaking changes to exported behavior unless the package is pre-1.0 and the
user explicitly waives it. openac is 0.0.0.9000 and the API needs room to
improve (e.g., the move toward tidy outputs).
**Decision:** The user explicitly waives the deprecation cycle until 1.0 —
the exported API may break freely between releases.
**Consequences:** Renames and return-shape changes need no soft-deprecation
period; revisit this decision at the 1.0 release.

### D-003 (2026-07-11): Principles pass — IP1 extended; IP2/IP3 adopted

**Context:** User interview stress-testing the original design principles
and considering new ones (IP changes require a recorded decision).
**Decision:** IP1 extended to also forbid writes outside user-specified or
`rappdirs` locations. Adopted IP2 (source media never modified/deleted) and
IP3 (no data egress without explicit opt-in; cloud tools permissible only
behind unmistakable opt-in). Also adopted GP5 (transparent calls) and GP6
(resilient batches); reworded GP1 (post-processing outputs in scope) and
GP2 (batch capability, not API shape).
**Consequences:** Future HuggingFace-style wrappers must run locally or
gate cloud use behind explicit opt-in; batch and high-level functions
converge on GP5/GP6 as they are touched.

### D-004 (2026-07-11): Reader family named `<tool>_read()`

**Context:** Planning the tidy-reader family (M01 openSMILE, M02 OpenFace,
M03 whisper). Two naming schemes were on the table: tool-prefix + verb
(`os_read`, `of_read`, `aw_read`) vs. reader-verb-first (`read_opensmile`,
`read_openface`, `read_whisper`). User leaned toward the first but was open
to the second.
**Decision:** Adopt `<tool>_read()` — `os_read`, `of_read`, `aw_read`. It
matches openac's dominant organizing convention (tool-family prefixes, per
DESIGN "Conventions"; cf. `os_extract`, `of_extract`, `aw_transcribe`), so a
user thinking "what can I do with openSMILE?" finds read alongside extract
under one prefix.
**Consequences:** All current and future output readers take the tool prefix.
`read_*` was considered and rejected for now; revisit at the 1.0 API freeze
(pre-1.0 the API may still break freely, D-002).

### D-005 (2026-07-11): Add `tibble` to Imports

**Context:** The reader family returns wide data frames; openSMILE
`emo_large` alone emits ~6500 feature columns, where a base `data.frame`
prints catastrophically to the console. Return shape was chosen as a wide
tibble at the M01 plan gate. `tibble` was not previously an Import (GP4 —
lean dependencies; guardrails require a decision for dependency changes).
**Decision:** Add `tibble` to Imports. Readers return true tibbles for
truncated printing and list-column ergonomics; the cost is one light Import.
**Consequences:** DESCRIPTION gains `tibble` (added in M01). Readers across
M01–M03 return tibbles; revisit only if a leaner return type is ever needed.
