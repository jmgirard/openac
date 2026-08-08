# M12: Does openac belong on top of tidymedia? — a fit assessment, and a decision

**Status:** done (2026-08-08, PR #13 https://github.com/jmgirard/openac/pull/13)

**Goal:** Determine, with cited evidence, whether openac should depend on
tidymedia for ffmpeg/ffprobe invocation and external-program discovery instead of
reimplementing them, and record the disposition as a decision.

**Outcome:** Declined the dependency. Added `cairn/references/tidymedia-fit.md`,
a fit assessment pinned to tidymedia `b99f7e8`: a 33-row overlap ledger (set O —
top-level assignments in the six ffmpeg/`programs_*` files, exported or not), an
8-row collision ledger (set C — the two `NAMESPACE` export sets intersected), an
invocation-layer comparison, and the distribution consequence; both sets computed
by script. Three findings decide it: ten of 33 symbols serve openface/opensmile/
whisper, outside tidymedia's scope; the packages use different `rappdirs` config
dirs; tidymedia's `find_program()` is unexported. `DESCRIPTION` untouched.

**Decisions:** D-016 (decline the dependency; harvest the boundary-quoting idea).

**Review:** 3 lenses + scorer, 24 findings, 2 actioned — R1 (85) a "twelve of 33"
count that is ten, wrong in D-016's first load-bearing reason; R6 (80) an
unmarked cross-repo `cairn/DESIGN.md` citation. Both fixed in place; 12
sub-threshold findings fixed too, 3 rejected. No criterion failed as written.
