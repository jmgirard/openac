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
