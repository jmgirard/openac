# M13: Quote at the process boundary, not at the call site

**Status:** done (2026-08-08, PR #14 https://github.com/jmgirard/openac/pull/14)

**Goal:** Move shell quoting out of the seven hand-written command builders and
into a single boundary runner, so no call site can ship a broken command.

**Outcome:** `R/run_tool.R` is the one place openac quotes: length decides the
contract (D-017), and `opt_arg()` replaces `ifelse(flag, " -x", "")`, which
tokenisation breaks. The four passthroughs delegate to it and all seven
assemblers emit token vectors. Fixes a live bug — `my $clip.mp4` reached tools
as `my .mp4`. The harness gained a `check_quoting` invariant plus
`boundary_argv`/`boundary_value`/`boundary_outfile` accessors.

**Decisions:** D-017 (the two-form passthrough contract).

**Review:** three lenses; blame-history and prior-PR both nil, diff-bug 12.
Actioned B1 (80, guard assumed one quote character); B2/B4/B6/B7/B8 fixed
though scored 70-78; B5 (Windows `%`) → candidate row + docs narrowed; B3
partly. One return (AC4's grep matched its own comment). Windows CI then caught
a unix-only assertion the review missed — vindicating B9, rejected at 48.
