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

## Open questions

- Whether the maintainer wants openac and tidymedia to share one config
  directory, given E-C4 below — this page records the divergence but does not
  propose a resolution — observed 2026-08-08.
