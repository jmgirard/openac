# M07: Wrapper testing contract — remainder and gated real invocations

**Status:** done (2026-08-07, PR #8 https://github.com/jmgirard/openac/pull/8)

**Goal:** Empty M06's deferral list and add GP7's second layer — real tool
invocations behind skip gates.

**Outcome:** The deferral list is empty; all 27 members of the computed `system2`
closure are enforced. New `R/utils.R` internals `dir_inputs`, `dir_outputs`,
`dir_walk` and `with_progress_mode` back all five `*_dir` wrappers, which now skip a
failing file with a warning and return a per-file outcome table — GP6 as a contract
rather than ad hoc. Path derivation fixed: anchored extension matching, no
`gsub(indir, …)` regex, directories excluded. `require_os()` guards the four suffixed
installers, resolving siblings from `getNamespaceExports()`; inert unexported
`install_openface_mac()` deleted. New test files cover installers, batch dirs, whisper
against a mocked `predict`, and `test-real-tools.R` (gated real invocations).

**Decisions:** AC2 amended at the implementation gate — `install_openface_mac` dropped
and `install_whisper` re-scoped, since neither constructs a download URL or install
path. `*_dir` return shape changed to an outcome table (D-002 permits the break).

**Review:** Three lenses, 23 findings; scorer actioned 3 — D9 (90) a test that could
not fail, D1 (88) `dir_inputs()` enumerating directories, D2 (82) the AC4 directory
test passing for the wrong reason — all fixed on the branch, 20 logged. CI green on
all five platforms; no lessons retired.
