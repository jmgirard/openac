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

### D-003 (2026-07-11): Principles pass — IP1 extended; IP2/IP3 adopted

**Context:** User interview stress-testing the original design principles
and considering new ones (IP changes require a recorded decision).
**Decision:** IP1 extended to also forbid writes outside user-specified or
`rappdirs` locations. Adopted IP2 (source media never modified/deleted) and
IP3 (no data egress without explicit opt-in; cloud tools permissible only
behind unmistakable opt-in). Also adopted GP5 (transparent calls) and GP6
(resilient batches); reworded GP1 (post-processing outputs in scope) and
GP2 (batch capability, not API shape).
**Consequences:** Future HuggingFace-style wrappers must run locally or
gate cloud use behind explicit opt-in; batch and high-level functions
converge on GP5/GP6 as they are touched.

### D-006 (2026-07-11): Add `purrr` to Imports, remove `pak`; whisper batch is sequential by design

**Context:** `R CMD check` flagged `purrr` used-but-undeclared
(`use_whisper.R:488` `purrr::pwalk`) and `pak` declared-but-unused. Planning
the check cleanup (M04). The `purrr::pwalk` in `aw_transcribe_dir()` is
deliberate: whisper is compute-intensive and `whisper.cpp` (via
`audio.whisper`) is already internally multi-threaded, so parallelizing
*across files* would oversubscribe the CPU or thrash a single GPU for no gain.
The CPU-bound `*_dir()` functions use `furrr::future_pwalk` (parallelizable).
**Decision:** Add `purrr` to Imports and keep the deliberately sequential
whisper loop (documented with a code comment). Remove `pak` from Imports
(unused in package code). Considered and rejected: replacing `purrr::pwalk`
with `furrr::future_pwalk` under a temporary `plan(sequential)` — more code, a
fragile plan save/restore, and no benefit given whisper's internal threading.
**Consequences:** DESCRIPTION gains `purrr`, drops `pak` (in M04). The
furrr-vs-purrr split now encodes parallelism intent (furrr = parallelizable,
purrr = deliberately sequential); note this in DESIGN when touched.

### D-004 (2026-07-11): Reader family named `<tool>_read()`

**Context:** Planning the tidy-reader family (M01 openSMILE, M02 OpenFace,
M03 whisper). Two naming schemes were on the table: tool-prefix + verb
(`os_read`, `of_read`, `aw_read`) vs. reader-verb-first (`read_opensmile`,
`read_openface`, `read_whisper`). User leaned toward the first but was open
to the second.
**Decision:** Adopt `<tool>_read()` — `os_read`, `of_read`, `aw_read`. It
matches openac's dominant organizing convention (tool-family prefixes, per
DESIGN "Conventions"; cf. `os_extract`, `of_extract`, `aw_transcribe`), so a
user thinking "what can I do with openSMILE?" finds read alongside extract
under one prefix.
**Consequences:** All current and future output readers take the tool prefix.
`read_*` was considered and rejected for now; revisit at the 1.0 API freeze
(pre-1.0 the API may still break freely, D-002).

### D-005 (2026-07-11): Add `tibble` to Imports

**Context:** The reader family returns wide data frames; openSMILE
`emo_large` alone emits ~6500 feature columns, where a base `data.frame`
prints catastrophically to the console. Return shape was chosen as a wide
tibble at the M01 plan gate. `tibble` was not previously an Import (GP4 —
lean dependencies; guardrails require a decision for dependency changes).
**Decision:** Add `tibble` to Imports. Readers return true tibbles for
truncated printing and list-column ergonomics; the cost is one light Import.
**Consequences:** DESCRIPTION gains `tibble` (added in M01). Readers across
M01–M03 return tibbles; revisit only if a leaner return type is ever needed.

### D-007 (2026-07-11): Tracking dir renamed `project/` → `cairn/`

**Context:** openac was scaffolded before the cairn plugin renamed its
tracking directory (upstream D-008: `project/` collides with RStudio
parlance and plausible pre-existing dirnames).
**Decision:** Rename `project/` → `cairn/` mid-pilot so the remaining pilot
milestones exercise the layout the plugin actually ships. Earlier D-entries
keep `project/` verbatim (append-only); archived milestones likewise.
**Consequences:** `.Rbuildignore` and CLAUDE.md updated; live milestone
mirrors now cite `cairn/ROADMAP.md`.

### D-008 (2026-07-11): Tidy-reader family API contract (RR01)

**Context:** `aw_read` (M03) is the third reader (`os_read`, `of_read`,
`aw_read`); the family's conventions were diverging with no stated contract and
a 1.0 API freeze approaching (D-002). A Fable review (RB01/RR01, archived)
examined input signature, column policy, value transformation, and shared shape.
**Decision:** Adopt a reader-family contract, to be written into DESIGN
"Conventions": (1) **Input forms** — a reader accepts every form its tool's
output natively exists in: a `file` path for file-only tools
(`os_read`/`of_read`); the in-memory object *plus* the `.rds`/`.csv` sidecars
the wrapper writes, as `x`, for R-native tools (`aw_read`); all accepted forms
yield identical output. (2) **Faithful-but-tidy columns** — pass through every
data-bearing column; drop only redundant re-encodings of retained columns
(`segment_offset` = `from` in ms may go; `speaker` must NOT — it is
diarization's payload, surfaced as a conditional column when present).
(3) **Lossless type-parsing only** — readers may parse encoded scalars into
natural R types (timestamp strings → numeric seconds) but never
filter/aggregate/derive; **time is numeric seconds** across all readers.
(4) Verbatim tool column names (mechanical whitespace cleanup only); grain
stated in each reader's first roxygen sentence. Rejected: teaching
`os_read`/`of_read` to accept objects (none exist); `aw_read` path-only (a disk
round-trip for nothing); retaining the timestamp strings; locking a
cross-reader `long=` shape pre-1.0 (heterogeneous outputs; additive later; GP4
`tidyr` pressure); a `speaker = TRUE` flag (the reader can see diarization).
**Consequences:** M03 gains a conditional `speaker` column, a CSV `colClasses`
parity fix, and a ≥1 h timestamp test; DESIGN "Conventions"/"Function Families"
gain the readers and this contract. Future readers inherit it. `long=TRUE`,
whisper `$tokens`, OpenFace block-subsetting, and a multi-file/`id`-column batch
idiom stay ROADMAP candidates (additive; D-002).

### D-009 (2026-07-11): Principles pass II — GP7–GP9 adopted; GP1 refined

**Context:** Design-interview deepening pass (facts committed 246b5fa: CRAN
quality bar, overwrite-`TRUE` convention, new warts). Candidate principles
came from the interview's banked ledger, git-history mining (the media-fix
streak: stream counting, audio-less files, video-vs-audio checks), and the
domain (methods-section reporting).
**Decision:** Adopt **GP7 — Two-layer testing** (mocked command-construction
everywhere + gated real invocations locally), **GP8 — Report what ran**
(broad framing: tool identity, version, and command recoverable at runtime;
narrow version-capture-only framing rejected), and **GP9 — Probe before
compute** (cheap media validation with a per-file message before the costly
tool; folding it into GP6 rejected — batch survival and pre-flight
diagnosis are separate testable claims). GP1 gains the earn-a-place clause
for post-read helpers. IP1–3 re-probed against the new facts: unchanged.
Rejected: IP-strength testing contract (unsatisfiable until the test
infrastructure exists — an IP that can't be met invites violations).
**Consequences:** DESIGN "Design Principles" reordered IP-block-first per
tracking rules; GP7/GP9 bind opportunistically as code is touched; GP8's
implementation is a ROADMAP candidate; future wrapper families inherit all
three from birth.

### D-010 (2026-08-07): GP7 layer 1 mocks `system2`; coverage is a computed gate

**Context:** Planning M06/M07 (the wrapper testing contract). Two choices
needed recording. (1) Which boundary layer-1 mocks: the four passthroughs
(`ffmpeg`/`ffprobe`/`openface`/`opensmile`) or `system2` itself. (2) Whether
"every tool-calling function has a command test" is enforced by a failing test
or merely reported — a question D-009 already touched when it rejected an
IP-strength testing contract, so an unrecorded hard gate would read as
contradicting it.
**Decision:** Layer 1 mocks **`base::system2`** via
`local_mocked_bindings(.package = "base")`, not the passthroughs. Mocking the
passthroughs was tried and rejected on evidence: the exported aliases `ffm`,
`ffp`, `of` and `os` are separate bindings to the same closures, so rebinding
`ffmpeg` does not intercept `ffm` — a probe confirmed `ffm("-x")` executing the
real ffmpeg binary under a passthrough mock. It also leaves the passthroughs'
own `system2` construction untested. Coverage is enforced by a **failing** test
whose domain is *computed* — a symbol-occurrence transitive closure over
`asNamespace("openac")` seeded at `system2` — minus a list of literal function
names deferred to a named milestone, carrying a staleness assertion so the list
cannot rot. Rejected: an advisory-only report (a gap that never fails is a gap
that never closes) and a hard gate over the whole closure (unsatisfiable while
M07's families are outstanding — the exact failure mode D-009 named).
**Consequences:** This is not the IP-strength contract D-009 rejected: it is a
GP7-level test, satisfiable the day it lands because the deferral list absorbs
what is not yet covered, and M07's acceptance is that the list empties. Any
future function that can reach an external tool fails the suite until it has a
command test — the enforcement point for GP7 on new wrapper families. A
call-head-only walk was rejected as the closure rule because `os_extract_dir`
and `aw_transcribe_dir` reach their tools through `do.call(what = …)` and are
invisible to it.

### D-011 (2026-08-07): Add `withr` to Suggests (test-only)

**Context:** M06's test harness needs per-test temporary directories and
scoped mocks across roughly six test files. GP4 keeps dependencies lean and the
guardrails require a question gate plus a decision for any dependency change,
including a test-only one.
**Decision:** Add `withr` to Suggests. It is already a hard dependency of
`testthat` (Imports), so every machine that can run openac's tests already has
it and the marginal install cost for users is zero; it is never loaded by
package code, only by tests. Considered and rejected: hand-rolled
`tempfile()` + `dir.create()` + `on.exit(unlink())` in each test — more code in
every file, and cleanup that fails open when a test errors before its
`on.exit()` is registered, which is how a test suite starts writing outside its
temp dir and quietly violates IP1.
**Consequences:** DESCRIPTION gains `withr` under Suggests (M06). Tests use
`withr::local_tempdir()` and friends for all filesystem and option scoping;
package code under `R/` still may not use it.

### D-012 (2026-08-07): Raise the testthat floor to 3.2.0

**Context:** M06's boundary harness calls
`testthat::local_mocked_bindings(.package = "base")`, and the suite also uses
`expect_no_match()`. DESCRIPTION declared `testthat (>= 3.0.0)`, which predates
both: `local_mocked_bindings()` arrived in 3.1.7 as an experimental function.
A check farm or user on an older testthat would fail every test file with
`could not find function "local_mocked_bindings"` — an R CMD check ERROR the
local run cannot reproduce. Surfaced as review-2 finding R7 (scored 75, below
the action threshold) and raised by the user at the merge gate.
**Decision:** Re-pin `testthat` in Suggests to `>= 3.2.0` — the release where
`local_mocked_bindings()` and `with_mocked_bindings()` became stable rather
than experimental (testthat 3.2.0 NEWS). testthat's NEWS never records when the
`.package` argument arrived, so the floor is set at the stability boundary,
which is at or above the true requirement. Considered and rejected: `>= 3.1.7`,
the release that introduced the function — that would declare a dependency on
an experimental API whose signature changed without a NEWS entry.
**Consequences:** DESCRIPTION Suggests carries the bound; testthat 3.2.0 is a
2023 release, so no practical burden. M07's tests inherit the same floor.

### D-013 (2026-08-08): The command contract's completeness precondition is observed and declared, never inferred

**Context:** D-010 enforces the command contract with a failing test, but that
test is only decidable over a complete suite run. M09 tried twice to infer
completeness from a content-derived proxy — harness install counts, then a text
search of test files — and both proxies diverged from the thing proxied, each
time leaving the gate silently disarmed. RB02/RR02 escalated the question.
**Decision:** completeness is established by OBSERVING which test files executed
(a `test_that` shadow recording at execution time) compared against a
content-free ground truth (the test files on disk), with the runner DECLARING a
full run via `OPENAC_FULL_SUITE` in `tests/testthat.R`. A declared-full run that
is incomplete FAILS; an undeclared partial run skips, naming the missing files;
and a canary in the contract file asserts its own recording in every run mode, so
a broken recorder fails the next run of any scope rather than skipping forever.
This rests on testthat's `filter` selecting whole files, never individual tests,
which makes "every test file executed" a biconditional for completeness rather
than a proxy. Considered and rejected: parse-tree detection of call sites (keeps
the content-proxy shape that failed twice), `testthat:::find_test_scripts()` and
other internals (fail open on reorganization, with no surfacing failure),
per-function enforcement over files that ran (undefinable for exactly the
uncovered functions the gate exists to catch), and moving the contract out of the
suite (advisory-only locally, which D-010 rejects).
**Consequences:** D-010's enforcement stands; its skip surface is now explicit.
A local filtered run skips the contract and says which files were missing; CI and
`R CMD check` declare full and therefore fail on incompleteness. Command tests
must never conditionally skip — the boundary is fully mocked, so a command test
that cannot run everywhere is a harness gap or an explicit `deferred` entry.
Parallel testthat is incompatible with the cross-file registry and is asserted
off by both routes (`Config/testthat/parallel` and `TESTTHAT_PARALLEL`). Because
the recording mechanism is a `test_that` shadow installed from the test helper,
this suite may only call `test_that()` bare: qualified `testthat::test_that()`,
and `describe()`/`it()`, bypass the shadow and are forbidden here.

### D-014 (2026-08-08): D-013's consequences describe machinery M10 lands, not M09 — annotating D-013

**Context:** D-013 was recorded when RR02 was ingested during M09, and its
Consequences paragraph is written in the present tense — a `test_that` shadow
recorder, `OPENAC_FULL_SUITE` declared in `tests/testthat.R`, a canary in the
contract file, `Config/testthat/parallel` asserted off by both routes, and a
prohibition on qualified `testthat::test_that()` / `describe()` / `it()` in this
suite. M09 was then re-cut: the coverage gate and everything RR02 binds moved to
M10, and M09 reverted `test-zzz-command-contract.R` to the default branch's
state. M09's review verified that none of that machinery exists in the merged
tree — `tests/testthat.R` is the stock stub, `DESCRIPTION` carries no
`Config/testthat/parallel`, and the contract file is byte-identical to the
default branch. So D-013 ships describing a tree that does not yet exist.
**Decision:** D-013 stands unedited and undisputed — the decision it records is
correct and IP4 forbids editing history — and this entry annotates it: every
present-tense claim in D-013's Consequences paragraph is a specification M10
implements, not a description of the tree at the time D-013 merged. A reader who
cannot find the recorder, the environment variable, the canary, or the parallel
assertions has found M10 unfinished, never a broken invariant. Considered and
rejected: holding D-013 out of M09 until M10 lands (a decision is recorded when
it is made, and RR02's answer was reached during M09); rewording D-013 in place
(IP4); and leaving it unannotated (M09's review reproduced the misreading it
invites, which is the whole reason this entry exists).
**Consequences:** D-013's operative content is unchanged and M10 remains its
implementor. Until M10 is `done`, the command contract on the default branch is
the pre-M09 one, whose skip-on-empty shape D-010 and D-013 both describe as
vacuous — a known gap, guarded only by M10's ROADMAP row and its dependency on
M09. This entry retires itself in effect once M10 merges: at that point D-013's
Consequences describe the tree, and this annotation is history explaining a
window that has closed.

### D-015 (2026-08-08): A test file may not skip before its tests run

**Context:** D-013 makes completeness an observation — a `test_that` shadow
records a file when one of its tests executes, compared against the test files
on disk. A file whose top level skips, which is the idiomatic whole-file
`skip_on_cran()`, executes no `test_that()` at all: nothing records it, and
`contract_decision()` reports it as a file that never ran. Under the
`OPENAC_FULL_SUITE` declaration CI and `R CMD check` set, that is a FAILURE for
a file behaving exactly as its author intended. M10's review reproduced it
(finding D20, scored 85) and left it as a ROADMAP candidate.
**Decision:** forbid the shape rather than widen the observation. A test file in
this suite may not skip outside a `test_that()` body. `top_level_skips()`
reports a skip call occurring anywhere in a top-level expression except within a
`test_that()` call or an unapplied function definition — so `if (cond) skip()`,
`local({ skip() })`, `suppressWarnings(skip_on_cran())`, an immediately-invoked
`(function() skip())()` and `do.call(skip, ...)` are caught, while
`gate <- function() skip()` is not — and `test-zzz-command-contract.R` fails
while it returns anything. D-013's
observation rule is untouched. Considered and rejected: recording the files
testthat SOURCES, via a `Reporter` subclass whose `start_file()` fires whatever
a file contains. It fixes the whole class rather than one form, but it needs
`R6` in Suggests, a fix for the ordering problem that `tests/testthat.R` runs
before testthat sources helpers (so the registry the observer was bound to is
rebuilt under it), and a widening of D-013's definition of the observation —
while every skip in this suite is already written inside a test, which is also
where GP7 asks for it. Promote the rejected approach on evidence that a real
whole-file skip is needed which per-test gating cannot express.
**Consequences:** this suite's forbidden-form list grows by one — alongside
qualified `testthat::test_that()`, `describe()` and `it()` (D-013), a skip
outside a `test_that()` body is a suite failure naming the file and telling the
author to move it inside. `test-real-tools.R` keeps its per-test
`skip_on_cran()` calls unchanged. Three holes are disclosed rather than closed,
all mirrors of the function-definition exclusion, all requiring a value a static
scan cannot resolve: a top-level call to a locally defined wrapper that itself
skips; a skip inside a lambda handed to an applier (`lapply(x, \(i) skip())`),
which is indistinguishable from a definition merely being stored; and a
`do.call()` whose callee is computed rather than named. The scanner's domain is
also the test files only — a `helper-*.R` or `setup-*.R` that skips is out of
scope, and aborts the run loudly rather than silently. Separately,
the declaration the gate's whole failure mode rests on is now asserted —
`declaration_present()` parses `tests/testthat.R` for its
`Sys.setenv(OPENAC_FULL_SUITE = ...)` call, because deleting that one line
turned every incompleteness into a silent pass and nothing anywhere noticed.

### D-016 (2026-08-08): Decline a tidymedia dependency; harvest the boundary-quoting idea instead

**Context:** openac and tidymedia (jmgirard/tidymedia, same maintainer) both
wrap ffmpeg/ffprobe and both implement external-program discovery, install, and
configuration. M12 assessed whether openac should depend on tidymedia rather
than reimplement — the full evidence is `cairn/references/tidymedia-fit.md`
(read at tidymedia `b99f7e8`), which enumerates 33 openac symbols across the six
ffmpeg/`programs_*` files and the 8 names both packages export. GP4 keeps
dependencies lean and the guardrails require a question gate plus a decision for
any dependency change.
**Decision:** Do not depend on tidymedia. openac keeps its own `find_program` /
`set_program` / `check_*` / `install_*` family and its own passthroughs. Three
things drove it. Ten of the 33 symbols serve openface, opensmile, or whisper,
which tidymedia's scope excludes — it is an FFmpeg/MediaInfo interface, and
while its D001 rules out full ffmpeg coverage and realtime use rather than other
tools by name, the exclusion follows from what the package is — so openac keeps a
full discovery-and-install family under any disposition and the dependency would
add a second mechanism beside the first, not replace it. The two packages read
and write different `rappdirs` config directories, so substituting tidymedia's
finders would silently strand every tool location an existing user recorded with
`openac::set_ffmpeg()`. And tidymedia's `find_program()` is unexported, leaving
no supported way to ask it to resolve openface or opensmile.
Considered and rejected: (a) a full dependency — rejected for the three reasons
above, plus it would add `Remotes: jmgirard/tidymedia` as a second independent
blocker on a CRAN gate `audio.whisper` already blocks, and tidymedia's D014
clean-break rename policy ships no deprecation shims pre-0.2.0; (b) depending on
tidymedia for the ffmpeg/ffprobe half only — rejected because it buys the
better quoting at the price of two discovery mechanisms, the config-migration
problem, and the `Remotes:` entry, when the quoting contract transfers as an
idea for roughly forty lines; (c) adopting tidymedia's `install_on_win` —
rejected as a regression, since it carries no OS guard and on macOS downloads and
extracts the Windows `.7z` before failing late when `set_program` refuses the
unresolvable `.exe` (`R/program_management.R:243`, `:147-149`) — wasted work and a
half-finished install where openac's `require_os()` (`R/programs_install.R:45`)
aborts before the first network call.
**Consequences:** `DESCRIPTION` is unchanged — no Imports, Suggests, or Remotes
entry is added. openac's CRAN gate gains no new blocker, keeping only the
`audio.whisper` `Remotes:` entry it already carries. The one
place tidymedia's design is ahead — quoting once at the process boundary rather
than by hand at each call site — becomes a ROADMAP candidate, alongside a
resilience adaptation and a row documenting the eight shared export names;
`cairn/references/tidymedia-fit.md` is the standing record and
should be re-read against tidymedia before any future session reopens this. This
declines a dependency, not the package: openac may still adopt tidymedia's ideas,
and a future reversal supersedes this entry rather than ignoring it.

### D-017 (2026-08-08): The passthrough argument contract — a lone string stays raw, a longer vector is tokens

**Context:** openac's four passthroughs (`ffmpeg`, `ffprobe`, `openface`,
`opensmile`) take one space-separated string and hand it to `system2()`, which
the shell then parses; every internal caller therefore hand-quotes file paths
with literal `"` characters at its own call site (`R/use_ffprobe.R:51-56` and six
siblings). D-016 harvested tidymedia's boundary-quoting idea as the one place its
design is clearly ahead, and M13 is where it lands — but adopting a token-vector
contract means deciding what the four *exported* functions accept, since they are
openac's documented way to call a tool by hand. A drafting audit found the first
cut unsatisfiable: a criterion admitting a path with a space and one rejecting a
stale multi-token string describe the same value — one character element carrying
whitespace — so no implementation could satisfy both.
**Decision:** Length decides. A length-1 argument is the legacy raw string and
reaches `system2()` untouched, exactly as today; a character vector of length > 1
is one CLI token per element, `shQuote()`-ed individually at the boundary with a
platform-appropriate `type`. Every internal assembler emits the vector form; the
raw string survives as a documented escape hatch.
Considered and rejected: (a) vector-only with a heuristic rejecting a lone
whitespace-carrying string that starts with `-` — rejected because the heuristic
is a rule of thumb the contract cannot state precisely, and it breaks every
existing user script for a defect none of them has; (b) vector-only with no
detection — rejected because a stale call becomes a single quoted argument and
the tool refuses it with its own message, turning a contract change into a
confusing tool error; (c) leaving the four functions alone and quoting only in a
new internal helper — rejected because it fixes openac's own call sites while
leaving the identical trap for any user who builds a command by hand, which is
what the passthroughs exist for. D-002's pre-1.0 free-break waiver would have
permitted (a) or (b); it was not needed, and the option that needs no break was
preferred over the options that spend one.
**Consequences:** No exported behavior changes for any call that works today —
the existing passthrough and alias tests pass unedited, which M13 makes its
no-break check (M13 T2). The two forms are documented together in each
passthrough's `@param arg` and in DESIGN's Architecture "Calling the CLIs"
paragraph, which currently states the single-string contract as the only one.
The cost is two behaviors in one function, mitigated by the rule being
mechanical rather than heuristic. This also unblocks GP5's remaining half: a
token vector is renderable as a human-readable command, so a display surface
becomes cheap — deliberately deferred out of M13 as a ROADMAP candidate rather
than folded in. A later 1.0 API freeze may retire the raw-string form; that
supersedes this entry rather than ignoring it.

### D-018 (2026-08-08): Add `curl` to Suggests (test-only), for the pinned-URL probe

**Context:** M16 ran the Windows installers against the live network and found
four patch-expert URLs answering HTTP 200 with a sign-in page. The test that
now watches for that class asks each pinned URL for its first two kilobytes and
inspects what comes back, which needs a ranged GET and access to the response
status, headers and body. `curl` entered `Suggests` during M16's
implementation as a fix for an `R CMD check` warning, without the question gate
and decision this repo requires for any dependency change (D-005, D-011; D-016
invoking the same rule to decline one). M16's review caught the omission and
returned the milestone; this entry is the gate held late.
**Decision:** Keep `curl` in Suggests. Considered and rejected:
`utils::download.file()` + `readBin()` — it is already in `Imports` via `utils`
and needs nothing added, but it exposes no status code, no headers and no range
request, so the probe would have to download each URL whole to inspect its
first bytes. That is roughly 600 MB per run instead of 18 KB, and it still
could not record the `Content-Range` total that AC2's per-URL size record is
read from. The probe would measure less at forty times the cost.
**Consequences:** DESCRIPTION gains `curl` under Suggests (M16). It is used by
`tests/testthat/test-installers-real.R` only, behind
`skip_if_not_installed("curl")` at every use site, so a machine without it
skips the probe rather than failing; package code under `R/` still may not use
it. `curl` is a hard dependency of much of the R toolchain already, so the
marginal install cost for a developer is near zero and for a user is nil —
the same reasoning D-011 recorded for `withr`.

### D-019 (2026-08-09): The skip channel is a non-error condition, and a nested skip stops at the call that raised it

**Context:** M18 gave the batch outcome table a third state. Two choices had
to be made, and both now bind code outside the milestone that made them: how a
single-file function tells `dir_walk()` it declined a file, and what happens
when the declining function is nested inside a larger per-file job. The second
was not settled at planning time — M18's first review round MEASURED that a
nested `overwrite = FALSE` skip inside `os_extract()` unwound the entire
per-file call, so openSMILE never ran, no CSV was written, and the row reported
a deliberate skip of work the caller did want done.
**Decision:** (1) A skip is signalled as a non-error condition of class
`openac_file_skipped` (`skip_file()`, `R/utils.R`). Considered and rejected: a
sentinel return value — `dir_walk()` inspects no return value, and the
`do.call` paths (`aw_transcribe_dir`, `os_extract_dir`) return heterogeneous
values a sentinel would have to be told apart from. Considered and rejected:
making a deliberate skip an error — re-running a completed batch would then
report every already-finished file as failed. (2) `status` describes the
batch's OWN job, so a skip raised by a nested prep call is absorbed at that
call (`absorb_skip()`, `R/utils.R`) and never reaches `dir_walk()`. Considered
and rejected: a calling handler in `dir_walk()` plus `rlang::cnd_muffle()` —
the work would resume, but the row would read `"skipped"` for a file that was
in fact processed, contradicting the definition M18 wrote for that word.
**Consequences:** The two `*_prep_audio_dir()` wrappers pass the prep function
to `dir_walk()` as `.f` directly, so their skip is the batch's own and reads
`"skipped"`; `os_extract_dir()` and `aw_transcribe_dir()` reuse an existing wav
and read `"ok"`. A direct call to any single-file function is unchanged,
because an unhandled `rlang::signal()` simply returns. A new skip site must
decide which of the two it is — and one added DEEPER than the prep call would
be absorbed and reported as `"ok"`, so it needs its own handling rather than
inheriting this one. The output-path-collision work parked on the ROADMAP is
the first consumer of the channel, and would extend the condition to carry the
offending path as a field.
