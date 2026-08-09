# M19: A guard that names no file

**Status:** done (2026-08-09, PR #20 https://github.com/jmgirard/openac/pull/20)

**Goal:** Make every input-file guard in the batch path say which file it
stopped on and what was wrong with it, and validate batch-wide arguments once
before the loop rather than once per file.

**Outcome:** the 38 batch-reachable guards (the `abort_file()` call sites in
`R/use_*.R`) route through a new `abort_file()` in `R/utils.R`, naming file and
defect on one plain line — the `error` column is data, so it formats eagerly
under `cli.num_colors = 1` and carries a `defect` field `dir_walk()` reads to
avoid naming the file twice. `check_file_arg()` rejects a non-scalar `infile`;
`match_formals()` resolves `do.call()`'s partial matching so `os_extract_dir()`
pre-flights the `config` the call will read. A missing intermediate wav is now
attributed to ffmpeg, a missing CSV to openSMILE.

**Decisions:** none promoted. Milestone-local: every guard inside a per-file
function rewritten, argument checks included; one shared helper over 38 bespoke
blocks; the pre-flight `config` default read from `formals(os_extract)`.

**Review:** three rounds — round 1 returned on AC1 and AC3 (7 of 18 actioned),
round 2 on two >=90 user-facing defects (4 of 20), round 3 actioned 2 of 20 and
returned nothing. No lesson retired; 2 captured, 2 stale ones pruned at the cap.
