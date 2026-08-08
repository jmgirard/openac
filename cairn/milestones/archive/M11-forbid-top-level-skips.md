# M11: A wholly-skipped test file cannot exist — the coverage gate's blind spot, closed at the door

**Status:** done (2026-08-08, PR #12 https://github.com/jmgirard/openac/pull/12)

**Goal:** Make a test file that skips before any `test_that()` runs impossible
to write, so the coverage gate's completeness observation can never mistake a
wholly skipped file for a file that never ran.

**Outcome:** `helper-openac.R` gains `top_level_skips(dir)` — any member of
`expected_test_files(dir)` whose top level holds a `skip`/`skip_*` call outside
a `test_that()` body or an unapplied `function` definition, so `if`/`local`/
`suppressWarnings` wrappers, an applied `(function() skip())()` and
`do.call(skip, ...)` are caught — and `declaration_present(path)`, which walks
`tests/testthat.R` in order for the `OPENAC_FULL_SUITE` state the run starts
with. Both asserted in `test-zzz-command-contract.R`, both mutation-verified;
fixtures in `test-harness-recording.R`. Test code only, nothing under `R/`.

**Decisions:** D-015 — forbid the shape rather than widen the observation,
D-013's execution-time rule untouched; the rejected `Reporter`-subclass
alternative is recorded there with its promotion condition.

**Review:** three lenses, 29 findings scored, 4 at ≥80 all fixed on the branch —
applied-function-expression hole (88), order-blind declaration check (87),
`do.call`-held callees (84), over-broad `^skip` prefix (80); 25 logged below
threshold. No lesson retired: M10 already covered the domain check reused here.
