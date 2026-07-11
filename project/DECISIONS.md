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
