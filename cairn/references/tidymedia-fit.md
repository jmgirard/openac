# Should openac depend on tidymedia for ffmpeg/ffprobe and program discovery? (M12)

**Provenance.** Ingested 2026-08-08 by M12 from two sources read read-only: this
repo at branch `m12-tidymedia-fit-assessment`, and the sibling repository
`/Users/jmgirard/GitHub/tidymedia` at commit `b99f7e875a016201178a9be01ab672b7ee77fdd2`
(working tree clean at read time). Both function sets below were computed by
script, not hand-listed; the script is reproduced under "Procedure" so any
reader can re-derive the counts.
Pagination: —.
Extraction: a 2026-08-08 snapshot; the assessed artifact has moved on independently since — observed 2026-08-08.

**Scope.** This is a fit assessment: it asks whether openac should take a
dependency on tidymedia for ffmpeg/ffprobe invocation and external-program
discovery, and it assembles the evidence a decision needs. It is not a summary
of tidymedia, not an evaluation of tidymedia's quality, and not a review of its
~50 task-recipe exports (`anonymize_video`, `picture_in_picture_batch`, …),
which M12 deliberately left out of scope. It builds no tooling and produces no
rule, so it names no test file.

This is a reference, not an authority — status lives in `ROADMAP.md`, decisions
in `DECISIONS.md`, architecture in `DESIGN.md`.

**Evidence snapshot.** Each line is a claim about repository state at read time,
not a standing fact.

- openac `R/use_ffmpeg.R`, `R/use_ffprobe.R`, `R/programs_{find,set,check,install}.R`, `NAMESPACE` — branch `m12-tidymedia-fit-assessment` — observed 2026-08-08.
- tidymedia `R/ffmpeg.R`, `R/ffprobe.R`, `R/ffm.R`, `R/program_management.R`, `NAMESPACE`, `DESCRIPTION`, `README.md` — commit `b99f7e8` — observed 2026-08-08.
- tidymedia advanced two commits (`ea4a9cf`, `b99f7e8`) during this milestone's own planning session — observed 2026-08-08.

## Procedure

Both sets are derived, never hand-listed. Re-run against the two repositories:

```r
six <- c("use_ffmpeg.R", "use_ffprobe.R", "programs_find.R",
         "programs_set.R", "programs_check.R", "programs_install.R")

## Set O — every symbol assigned at top level in those six files, exported or
## not, `function(` or alias.
top_assigns <- function(path) {
  out <- character(0)
  for (e in parse(path, keep.source = FALSE)) {
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
        is.name(e[[2]])) {
      rhs <- e[[3]]
      kind <- if (is.call(rhs) && identical(as.character(rhs[[1]]), "function")) "function"
              else if (is.name(rhs)) paste0("alias of ", as.character(rhs))
              else "data"
      out <- c(out, stats::setNames(kind, as.character(e[[2]])))
    }
  }
  out
}
O <- unlist(lapply(file.path("<openac>/R", six), top_assigns))

## Set C — the intersection of the two packages' export() symbol sets.
exports_of <- function(repo) {
  ns <- readLines(file.path(repo, "NAMESPACE"), warn = FALSE)
  m  <- regmatches(ns, regexec("^export\\(([^)]+)\\)$", ns))
  s  <- vapply(m, function(x) if (length(x) == 2) x[[2]] else NA_character_, "")
  sort(gsub('"', "", s[!is.na(s)]))
}
C <- sort(intersect(exports_of("<openac>"), exports_of("<tidymedia>")))
```

Measured at the commits in the Provenance block: `length(O) == 33` (24 exported,
9 internal); openac exports 48 symbols, tidymedia 86; `length(C) == 8`.

## What tidymedia is

A neutral characterization, before any verdict.

tidymedia is an R interface to FFmpeg and MediaInfo for reproducible media
preprocessing — batch trimming, cropping, format standardization, and metadata
extraction as tibbles (`cairn/DESIGN.md:9-16`). It is deliberately not "all of
ffmpeg in R" (its D001). It is organized in three layers: raw CLI passthroughs
(`ffmpeg()`, `ffprobe()`, `mediainfo()`), an `ffm_*` command builder, and task
verbs on top.

Its ffmpeg-facing machinery is materially more developed than openac's in two
places. Argument assembly is a structured pipeline — `ffm_args()`
(`R/ffm.R:1164`) produces an unquoted token vector and `ffm_compile()`
(`R/ffm.R:1152`) produces the human-readable command from the same structure, so
the displayed command and the executed one cannot drift. And execution is
centralized in `run_program()` (`R/program_management.R:108-122`), which
`shQuote()`s every token with a platform-appropriate type
(`R/program_management.R:119`) and aborts on a missing binary.

It is version `0.1.0.9000`, badged lifecycle-experimental (`README.md:8-9`),
installable only via `devtools::install_github()` (`README.md:29`), has never
cut a release (no git tags), and carries an explicit clean-break rename policy
with no deprecation shims (its D014). It is under very active development.

## Overlap ledger — set O mapped to tidymedia

Tags: `Adopt` (tidymedia's is better and openac should use it) · `Adapt`
(tidymedia has the idea, openac would need to change it) · `Already have`
(equivalent; no gain) · `Reject` (openac's is better, or tidymedia cannot serve
it).

| # | openac symbol | kind, file:line | exported | tidymedia counterpart | Tag |
|---|---|---|---|---|---|
| E1 | `ffmpeg` | function, `R/use_ffmpeg.R:19` | yes | `ffmpeg` (`R/ffmpeg.R:20`) | Reject |
| E2 | `ffm` | alias of `ffmpeg`, `R/use_ffmpeg.R:32` | yes | `ffm` (`R/ffm.R:63`) — **same name, different meaning** | Reject |
| E3 | `ffprobe` | function, `R/use_ffprobe.R:19` | yes | `ffprobe` (`R/ffprobe.R:19`) | Already have |
| E4 | `ffp` | alias of `ffprobe`, `R/use_ffprobe.R:32` | yes | no counterpart | Already have |
| E5 | `ffp_count_streams` | function, `R/use_ffprobe.R:46` | yes | `probe_streams` (`R/ffprobe.R:259`) | Adapt |
| E6 | `find_program` | function, `R/programs_find.R:13` | yes | no counterpart — tidymedia's `find_program` (`R/program_management.R:18`) is **not exported** | Reject |
| E7 | `require_program` | function, `R/programs_find.R:68` | no | no counterpart — internal analogue `run_program` (`R/program_management.R:108`) | Adapt |
| E8 | `find_ffmpeg` | function, `R/programs_find.R:84` | yes | `find_ffmpeg` (`R/program_management.R:70`) | Reject |
| E9 | `find_ffprobe` | function, `R/programs_find.R:94` | yes | `find_ffprobe` (`R/program_management.R:78`) | Reject |
| E10 | `find_openface` | function, `R/programs_find.R:104` | yes | no counterpart — out of tidymedia's scope | Reject |
| E11 | `find_opensmile` | function, `R/programs_find.R:114` | yes | no counterpart — out of tidymedia's scope | Reject |
| E12 | `set_program` | function, `R/programs_set.R:11` | yes | `set_program` (`R/program_management.R:141`) | Adapt |
| E13 | `set_ffmpeg` | function, `R/programs_set.R:33` | yes | `set_ffmpeg` (`R/program_management.R:174`) | Reject |
| E14 | `set_ffprobe` | function, `R/programs_set.R:43` | yes | `set_ffprobe` (`R/program_management.R:180`) | Reject |
| E15 | `set_openface` | function, `R/programs_set.R:53` | yes | no counterpart — out of scope | Reject |
| E16 | `set_opensmile` | function, `R/programs_set.R:63` | yes | no counterpart — out of scope | Reject |
| E17 | `check_ffmpeg` | function, `R/programs_check.R:14` | yes | no counterpart — tidymedia has no `check_*` family | Reject |
| E18 | `check_ffprobe` | function, `R/programs_check.R:39` | yes | no counterpart | Reject |
| E19 | `check_openface` | function, `R/programs_check.R:65` | yes | no counterpart | Reject |
| E20 | `check_opensmile` | function, `R/programs_check.R:91` | yes | no counterpart | Reject |
| E21 | `installer_suffixes` | data, `R/programs_install.R:10` | no | no counterpart | Reject |
| E22 | `sysname_labels` | data, `R/programs_install.R:13` | no | no counterpart | Reject |
| E23 | `tool_labels` | data, `R/programs_install.R:14` | no | no counterpart | Reject |
| E24 | `current_sysname` | function, `R/programs_install.R:18` | no | no counterpart | Reject |
| E25 | `label_sysname` | function, `R/programs_install.R:20` | no | no counterpart | Reject |
| E26 | `label_tool` | function, `R/programs_install.R:24` | no | no counterpart | Reject |
| E27 | `installers_for` | function, `R/programs_install.R:31` | no | no counterpart | Reject |
| E28 | `require_os` | function, `R/programs_install.R:45` | no | no counterpart — tidymedia has **no OS guard at all** | Reject |
| E29 | `install_ffmpeg_win` | function, `R/programs_install.R:90` | yes | `install_on_win` (`R/program_management.R:213`) | Reject |
| E30 | `install_openface_win` | function, `R/programs_install.R:142` | yes | no counterpart — out of scope | Reject |
| E31 | `install_opensmile_win` | function, `R/programs_install.R:229` | yes | no counterpart — out of scope | Reject |
| E32 | `install_opensmile_mac` | function, `R/programs_install.R:289` | yes | no counterpart — out of scope | Reject |
| E33 | `install_whisper` | function, `R/programs_install.R:355` | yes | no counterpart — out of scope | Reject |

Twelve of the 33 (E10, E11, E15, E16, E17–E20 in part, E30–E33) concern openface,
opensmile, or whisper, which tidymedia's stated scope will never cover. openac
therefore keeps a discovery and installation mechanism under every disposition;
a dependency would add a second one beside it, not remove the first.

## Paired-row differences

The ten rows above that name a counterpart, each with the three facts a
dependency decision turns on. "Config dir" is the `rappdirs` directory the
function reads or writes; where a function reaches one only through a callee,
the callee is named. "Failure signal" is what happens when the tool cannot be
resolved. "Quoting" is how arguments reach the process boundary.

| Row | Config dir (openac → tidymedia) | Failure signal (openac → tidymedia) | Quoting (openac → tidymedia) | Verdict |
|---|---|---|---|---|
| E1 `ffmpeg` | reads none directly; reaches `user_config_dir("openac","R")` via `require_program` → `find_program` (`R/programs_find.R:26`) → reaches `user_config_dir("tidymedia","R")` via `find_ffmpeg` (`R/program_management.R:29`) | `cli::cli_abort("Can't run …")` in `require_program` (`R/programs_find.R:71`) → `find_ffmpeg()` warns and returns `NULL`, `glue()` collapses to `character(0)`, and `system()` raises the base error "non-empty character argument expected" (`R/ffmpeg.R:28`; observed 2026-08-08) | one caller-assembled string to `system2()` with the resolved path as `command`, so the path is never shell-parsed (`R/use_ffmpeg.R:23`) → the path is interpolated **unquoted** into a `system()` string (`R/ffmpeg.R:28`), so an ffmpeg under a path containing a space fails (observed 2026-08-08) | openac's is safer on both axes |
| E2 `ffm` | n/a — openac's is an alias of `ffmpeg`; tidymedia's is an alias of `ffm_files`, a pipeline constructor that touches no binary (`R/ffm.R:63`) | as E1 → n/a, constructs an object | as E1 → n/a | **name collision, incompatible meanings** |
| E3 `ffprobe` | as E1 → as E1 (`R/program_management.R:29`) | `cli_abort` via `require_program` (`R/programs_find.R:71`) → base error as E1 (`R/ffprobe.R:21`) | one caller-assembled string to `system2()` (`R/use_ffprobe.R:23`) → path **is** quoted here (`R/ffprobe.R:21`), unlike `ffmpeg()` — an asymmetry internal to tidymedia | equivalent; tidymedia's quoting is inconsistent between its own two passthroughs |
| E5 `ffp_count_streams` | none directly → none directly | `stopifnot(file.exists(infile))` (`R/use_ffprobe.R:48`), then the E3 signal → `probe_all()` returns an all-`NA` row and warns rather than aborting (`R/ffprobe.R:119-124`) | caller hand-quotes the path with literal `"` inside the arg string (`R/use_ffprobe.R:51-56`) → token vector through `run_program`'s `shQuote` (`R/program_management.R:119`) | tidymedia's is structurally better: typed tibble out, per-token quoting, resilient to bad files |
| E7 `require_program` | via `find_program` (`R/programs_find.R:26`) → via the `location` argument its caller resolved (`R/program_management.R:108`) | `cli_abort` naming the tool (`R/programs_find.R:71`) → `cli_abort("Could not locate {program}.")` (`R/program_management.R:110-112`) | n/a — resolves, does not invoke → applies `shQuote` per token and calls `system2` (`R/program_management.R:119`) | same guard; tidymedia's also owns quoting, openac's does not |
| E8 `find_ffmpeg` | `user_config_dir("openac","R")` (`R/programs_find.R:26`) → `user_config_dir("tidymedia","R")` (`R/program_management.R:29`) | `cli_warn` + `NULL`, twice — absent, and recorded-but-vanished (`R/programs_find.R:31,47`) → `cli_warn` + `NULL`, same two cases (`R/program_management.R:37,48`) | n/a — resolves, does not invoke → n/a | **different config directories**; openac additionally returns an absolutized, unnamed path (`R/programs_find.R:57`) where tidymedia returns `Sys.which()`'s named result or the raw config line |
| E9 `find_ffprobe` | as E8 | as E8 | n/a | as E8 |
| E12 `set_program` | writes `user_config_dir("openac","R")` (`R/programs_set.R:17`) → writes `user_config_dir("tidymedia","R")` (`R/program_management.R:152`) | base `stopifnot()` on all three arguments (`R/programs_set.R:13-15`) → `rlang::arg_match` + `check_string` + `cli_abort("Can't find an executable at …")` (`R/program_management.R:145-149`) | n/a → n/a | tidymedia's conditions are better (cli, per DESIGN Conventions); openac's `stopifnot` is the legacy style DESIGN marks for opportunistic migration |
| E13 `set_ffmpeg` | as E12 | as E12 | n/a | as E12 |
| E14 `set_ffprobe` | as E12 | as E12 | n/a | as E12 |
| E29 `install_ffmpeg_win` | writes `user_data_dir("openac","R")/ffmpeg` (`R/programs_install.R:98`) → writes `user_data_dir("tidymedia","R")/ffmpeg` (`R/program_management.R:220`) | `require_os()` aborts with class `openac_wrong_os` **before any network call** (`R/programs_install.R:62-70`) → **no OS guard**: on macOS or Linux it downloads the Windows `.7z` and records `bin/ffmpeg.exe` paths (`R/program_management.R:213-248`) | n/a → n/a | openac's is strictly safer; this is the exact silent wrong-install openac's guard was added to stop |

Two findings dominate this table.

**The config directories differ** (E8, E9, E12–E14). A tool a user registered
with `openac::set_ffmpeg()` is written to `user_config_dir("openac", "R")` and is
invisible to tidymedia's finders, which read `user_config_dir("tidymedia", "R")`
— and the reverse. Depending on tidymedia for discovery would therefore silently
strand every existing openac user's configuration, unless openac also migrates
the file or writes both. This is not a detail a dependency absorbs; it is
user-visible breakage.

**tidymedia's `find_program()` is not exported** (E6). It is defined at
`R/program_management.R:18` with roxygen but no `@export`, and is absent from
tidymedia's `NAMESPACE`. openac could not call it without `:::`, which is not a
supported interface. The four exported `find_*` wrappers cover ffmpeg, ffprobe,
ffplay, and mediainfo only, so there is no supported way to ask tidymedia to
resolve openface or opensmile.

## Name-collision ledger — set C

The eight symbols both packages export, from the `NAMESPACE` intersection under
"Procedure". This matters because a user who attaches both packages gets one of
each name, decided by attach order, with no warning beyond R's masking message.

| # | Symbol | openac meaning | tidymedia meaning | Agree? |
|---|---|---|---|---|
| C1 | `ffm` | alias of `ffmpeg`, the CLI passthrough (`R/use_ffmpeg.R:32`) | alias of `ffm_files`, a pipeline **constructor** returning an object (`R/ffm.R:63`) | **No — incompatible.** `ffm("-version")` returns tool output under openac and constructs a job object under tidymedia |
| C2 | `ffmpeg` | passthrough; one arg string; `system2` with resolved path (`R/use_ffmpeg.R:19-24`) | passthrough; one arg string; `system()` with unquoted interpolated path (`R/ffmpeg.R:20-28`) | Partly — same role and calling shape, different boundary mechanism and quoting safety |
| C3 | `ffprobe` | passthrough (`R/use_ffprobe.R:19-24`) | passthrough; path quoted (`R/ffprobe.R:19-21`) | Partly — as C2, but tidymedia quotes here |
| C4 | `find_ffmpeg` | resolves via `user_config_dir("openac","R")`; returns absolutized unnamed path (`R/programs_find.R:84`, `:26`, `:57`) | resolves via `user_config_dir("tidymedia","R")`; returns `Sys.which()`'s named result or the raw config line (`R/program_management.R:70`, `:29`) | **No — different config directory and different return shape** |
| C5 | `find_ffprobe` | as C4 (`R/programs_find.R:94`) | as C4 (`R/program_management.R:78`) | **No — as C4** |
| C6 | `set_ffmpeg` | writes `user_config_dir("openac","R")` (`R/programs_set.R:33`, `:17`) | writes `user_config_dir("tidymedia","R")` (`R/program_management.R:174`, `:152`) | **No — writes a different file** |
| C7 | `set_ffprobe` | as C6 (`R/programs_set.R:43`) | as C6 (`R/program_management.R:180`) | **No — as C6** |
| C8 | `set_program` | `program` domain is ffmpeg/ffprobe/openface/opensmile; base `stopifnot` validation (`R/programs_set.R:11-15`) | `program` domain is ffmpeg/ffprobe/ffplay/mediainfo; `arg_match` + `cli_abort` (`R/program_management.R:141-149`) | **No — different accepted values and different config file** |

Six of the eight disagree outright, and `ffm` (C1) disagrees most sharply: same
name, same package prefix convention, entirely different semantics and return
type. Nothing in either package warns about this today.

## Invocation layers compared

Both packages assemble an ffmpeg command and hand it to a process, but they
split the work at different seams.

**openac.** A typed high-level function (`os_prep_audio()`, `aw_prep_audio()`)
validates its named parameters and assembles a **single space-separated string**,
hand-quoting any file path with literal `"` characters (the pattern is visible at
`R/use_ffprobe.R:51-56`). That string goes to the passthrough `ffmpeg()`
(`R/use_ffmpeg.R:19`), which resolves the binary through `require_program()`
(`R/programs_find.R:68`) and calls `system2(<resolved path>, args = arg)`
(`R/use_ffmpeg.R:23`). The argument assembler and the boundary are the same
function; there is no separate representation of the command, which is why
DESIGN records GP5 ("Transparent calls") as unmet.

**tidymedia.** An `ffm_*` builder accumulates a job object; `ffm_args()`
(`R/ffm.R:1164`) renders it to an **unquoted token vector**, one element per CLI
argument, and `ffm_compile()` (`R/ffm.R:1152`) renders the same structure to the
human-readable command string — so what is displayed and what is executed derive
from one source and cannot drift. `ffm_run()` (`R/ffm.R:1381`) passes the token
vector to `run_program()` (`R/program_management.R:108`), which `shQuote()`s
every token with a platform-appropriate type (`R/program_management.R:119`) and
aborts if the binary is missing (`:110-112`).

The seam difference is the substantive one. openac quotes at the *caller*, once
per call site, by hand; tidymedia quotes at the *boundary*, once, mechanically.
openac's approach puts a quoting bug one forgotten `"` away at every call site;
tidymedia's centralizes it — and tidymedia's own two Layer-0 escape hatches
(`R/ffmpeg.R:28` unquoted, `R/ffprobe.R:21` quoted) demonstrate the failure mode
by bypassing their own boundary and disagreeing with each other.

This is the one place where tidymedia's design is clearly ahead of openac's, and
it is available as an idea without a dependency: the token-vector +
`shQuote`-at-the-boundary contract is roughly forty lines and rests on nothing
tidymedia-specific.

## Distribution consequence

All figures read at commit `b99f7e8` — observed 2026-08-08.

tidymedia is `Version: 0.1.0.9000`, `License: GPL-3` (`DESCRIPTION:4,14`), carries
no `Remotes:` field of its own, has no git tags at all, is badged
lifecycle-experimental (`README.md:9`), and its only documented installation
route is `devtools::install_github("jmgirard/tidymedia")` (`README.md:29`). It is
not on CRAN and has never cut a release.

A hard `Imports: tidymedia` would therefore require `Remotes: jmgirard/tidymedia`
in openac's `DESCRIPTION`. openac already carries `Remotes: bnosac/audio.whisper`
(`DESCRIPTION:35-36`), and `cairn/DESIGN.md` "Purpose & Scope" states the CRAN
gate plainly: submission is blocked until there is "a CRAN-legal resolution for
`audio.whisper` (the `Remotes:` field cannot ship …)".

The honest reading is narrower than "this blocks CRAN": the gate is *already*
closed by `audio.whisper`, so a tidymedia dependency does not close a gate that
is open. What it does is **add a second, independent blocker to a gate that
currently has one**, each needing its own CRAN-legal resolution before
submission. The `audio.whisper` blocker has at least two known exits already
under consideration (Additional_repositories, or wrapping whisper.cpp directly);
a tidymedia blocker's only exits are tidymedia reaching CRAN, or openac vendoring
what it needs. Both are outside openac's control in a way the current blocker is
not — `audio.whisper` is a third-party package openac chose to wrap, whereas
tidymedia is the same maintainer's package, which cuts both ways: the release
timing is in the maintainer's hands, but so is every clean-break rename.

That last point is the sharpest cost. tidymedia's D014 is an explicit
clean-break rename policy with no `lifecycle` shims — old names are removed, not
deprecated — and its own DESIGN records the naming as still unsettled pending a
follow-up milestone. openac depending on it pre-0.2.0 buys a dependency whose
function and argument names may change without a deprecation window, in a
package whose maintainer has deliberately reserved the right to do so.

## Disposition

Where every row above lands. The dependency decision itself is not this page's
to make — it is recorded as **D-016** (2026-08-08): decline the dependency,
harvest the boundary-quoting idea.

**Rows tagged `Reject` (28 of 33), with their reason.** E10, E11, E15–E20,
E30–E33 concern openface, opensmile, or whisper, which tidymedia's stated scope
excludes — a dependency cannot serve them and openac keeps its own machinery.
E21–E28 are internal helpers of openac's installer guard with no tidymedia
analogue; E28 (`require_os`) is rejected in the strong sense that tidymedia's
`install_on_win` has no OS guard at all and adopting it would be a regression.
E1, E2, E6, E8, E9, E13, E14, E29 are rejected because the config-directory
divergence and the unexported `find_program` make them non-substitutable without
user-visible breakage.

**Rows tagged `Adapt` (3): E5, E7, E12.** These are ideas worth having, none of
which requires the dependency. They route to the ROADMAP:

- E7 and the invocation-layer finding → a new candidate row, verbatim:
  `- Quote at the process boundary, not at the call site — adopt a token-vector + shQuote contract in the passthroughs (the pattern tidymedia centralizes in run_program) so a forgotten literal quote at one call site cannot ship a broken command; today every caller hand-quotes paths (R/use_ffprobe.R:51-56) — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, E7)`
- E12 → absorbed into the existing DESIGN-recorded convention rather than a new
  row: `cairn/DESIGN.md` Conventions already states that legacy `stopifnot()` +
  base `warning()` "migrates opportunistically when touched", which covers
  `set_program`'s validation style. No new row; no separate tracking needed.
- E5 → a new candidate row, verbatim:
  `- Make ffp_count_streams resilient rather than fatal — it aborts on an unreadable file (R/use_ffprobe.R:48) where a batch would rather get NA and a warning, as tidymedia's probe_all does (R/ffprobe.R:119-124); relates to GP6/GP9 — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, E5)`

**Collision ledger C1–C8 → a new candidate row**, verbatim:
`- Document that openac and tidymedia share 8 exported names of which 6 disagree (ffm most sharply: passthrough alias vs. job constructor), so attaching both masks silently — a README/vignette note, or a rename at the 1.0 API freeze — added 2026-08-08 — M12 (cairn/references/tidymedia-fit.md, C1-C8)`

**The config-directory divergence** is deliberately not routed to a row here: it
is a consequence of the dependency decision, not an independent idea, and M12's
scope routes it only if the decision makes it live.

This page produced no rule, so it names no test file.

## Open questions

- Whether openac and tidymedia should share one config directory (C4–C7) was
  open when this page was drafted; D-016 declined the dependency, which makes it
  moot for now — the two families stay independent, and a user of both records
  each tool twice. Re-opens only if the dependency question re-opens — resolved
  and re-checked 2026-08-08.
- This page's verdicts rest on tidymedia at `b99f7e8`, which advanced twice
  during M12 itself; any future session reopening D-016 re-reads tidymedia
  before relying on the ledgers above — observed 2026-08-08.
