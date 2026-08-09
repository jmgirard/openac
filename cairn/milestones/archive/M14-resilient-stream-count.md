# M14: A bad file is an outcome, not the end of the batch

**Status:** done (2026-08-08, PR #17 https://github.com/jmgirard/openac/pull/17)

**Goal:** Make `ffp_count_streams()` report an unprobeable file rather than abort
on it, so a batch records that file as a failed row instead of dying on it.

**Outcome:** `ffp_count_streams()` returns `NA` counts with a warning naming the
file, for a nonexistent path and for a non-zero ffprobe exit read from the
`status` attribute `system2()` sets; a non-scalar `infile` and a missing ffprobe
still abort. Its callers dispose of `NA`: the two `*_check_audio` predicates
return `FALSE` before their second query, `aw_prep_audio` aborts naming the file
so `dir_walk()` records the row, `aw_transcribe` skips. R's own exit-status
warning is suppressed by position, never by text, so it holds on a localized R.

**Decisions:** none milestone-local; the caller-by-caller `NA` split and the
three question-gate choices are work-log entries.

**Review:** two rounds. Round 1 returned it (AC4 unmet) on A1 (93) locale-
dependent suppression and A4 (92)/A17 (88) NEWS and DESIGN claiming a per-file
failure two of four batch paths lack, plus A3 (90), A9 (87), A2 (85). Round 2
caught the fix reintroducing that same unverified-prose defect — F4 (90), DESIGN
describing a message naming no file — plus F1 (92), F10 (85), F5 (82), F8 (80).
All actioned fixed, 31 sub-threshold logged, and a candidate opened for the two
batch tables recording a skipped file as a success.
