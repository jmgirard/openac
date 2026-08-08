# M12: Does openac belong on top of tidymedia? — a fit assessment, and a decision

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP3, GP4
- **Branch/PR:** `m12-tidymedia-fit-assessment`

## Goal

Determine, with cited evidence, whether openac should depend on tidymedia for
ffmpeg/ffprobe invocation and external-program discovery instead of
reimplementing them, and record the disposition as a decision.

## Scope

**In:** A committed fit assessment in `cairn/references/` comparing openac's
ffmpeg/ffprobe and `programs_*` surface against tidymedia's, covering the
functions both packages export and how each package actually invokes ffmpeg;
the distribution consequence of a hard dependency; a recorded decision; and
ROADMAP rows for whatever the assessment surfaces and this milestone does not do.

**Out:**
- Executing an adopt decision — editing `DESCRIPTION`, rewriting call sites,
  reworking tests → its own milestone, planned immediately after this one and
  depending on it, if and only if the decision is to adopt.
- Any runtime code change. This milestone ships no change under `R/`.
- Assessing tidymedia's ~50 task-recipe exports (`anonymize_video`,
  `picture_in_picture_batch`, …) for openac's use → ROADMAP candidate if the
  assessment finds cause to want one.
- Changing openac's config-directory or discovery behavior in response to what
  the assessment finds → ROADMAP candidate row.

## Acceptance criteria

- [ ] AC1. `cairn/references/tidymedia-fit.md` exists, authored from
  `templates/synthesis-note.md`, and `cairn_validate` reports no FAIL for its
  `references` check. Read directly from the file: its `**Provenance.**` block
  names the tidymedia commit SHA the assessment read, and its `Extraction:`
  status uses the template's snapshot alternative (the assessed artifact has
  moved on independently since) carrying its own `— observed` date — not the
  `nothing to re-verify` alternative, which would exempt the page from
  staleness permanently. The note states that it produced no rule and names no
  test file, because this milestone ships no code.
- [ ] AC2. The note contains an overlap table with one row per member of set O,
  where O is enumerated by this procedure: every symbol assigned at top level in
  `R/use_ffmpeg.R`, `R/use_ffprobe.R`, `R/programs_find.R`, `R/programs_set.R`,
  `R/programs_check.R`, or `R/programs_install.R` — whether exported or not, and
  whether assigned a `function(` or an alias. Each row names the tidymedia
  export it pairs with, drawn from tidymedia's `NAMESPACE`, or records "no
  counterpart".
- [ ] AC3. Every row of AC2's table naming a tidymedia counterpart carries a
  difference verdict citing `file:line` in both repos, and states three things:
  which `rappdirs` directory the function reads or writes, how it signals an
  unresolvable tool, and how it quotes arguments at the process boundary. Where
  the answer for a row is raised in a different function, the citation points at
  that function rather than the row saying the question does not apply.
- [ ] AC4. The note records what a hard dependency would do to distribution, or
  records that it would do nothing: tidymedia's release status as read from its
  `DESCRIPTION` and `README.md` at the commit named in the Provenance block,
  whether a `Remotes:` entry would be required, and how that interacts with the
  CRAN gate stated in `cairn/DESIGN.md` "Purpose & Scope" and with the
  `Remotes:` entry `DESCRIPTION` already carries.
- [ ] AC5. The note contains a name-collision ledger with one row per member of
  set C, where C is the intersection of the symbol sets appearing in `export()`
  lines of the two packages' `NAMESPACE` files — computed by intersecting the
  two files, never hand-listed. Each row states whether the two meanings agree,
  citing `file:line` in both repos. The note also characterizes, in prose,
  tidymedia's ffmpeg-invocation layer against openac's, naming for each the
  function that assembles arguments and the function that reaches the process
  boundary.
- [ ] AC6. `cairn/DECISIONS.md` gains an entry recording the dependency
  disposition chosen by the user at this milestone's gate, naming the
  disposition, the alternative(s) considered and rejected, and the consequence
  for openac's `DESCRIPTION` — following D-005, D-006, D-011, D-012.
- [ ] AC7. For every item in the note's "Disposition" section routed to the
  ROADMAP, the note quotes verbatim either the bullet text added under
  `## Candidates` or the existing bullet it was absorbed into, and that text is
  present in `cairn/ROADMAP.md`.

## Coverage

- AC1 → T5
- AC2 → T1, T5
- AC3 → T2, T5
- AC4 → T3, T5
- AC5 → T1, T4, T5
- AC6 → T6
- AC7 → T6

## Tasks

- [x] T1. Compute set O (AC2's procedure over the six openac files) and set C
      (the two `NAMESPACE` export-set intersection, AC5); record both lists and
      the tidymedia commit SHA read. Both are scripted, not hand-listed.
- [x] T2. For each paired row, write the difference verdict with `file:line` in
      both repos — config directory, failure signal, quoting contract. openac's
      failure signal for the passthroughs is raised in `require_program()`
      (`R/programs_find.R:68`); tidymedia's is in `run_program()`
      (`R/program_management.R:108`) for structured calls and nowhere for its
      `system()`-based escape hatches.
- [x] T3. Write the distribution section: tidymedia release status at the pinned
      commit, `Remotes:` consequence, interaction with the CRAN gate
      (`cairn/DESIGN.md` "Purpose & Scope") and the existing `Remotes:` entry
      (`DESCRIPTION:35-36`).
- [x] T4. Write the collision verdicts for set C and the invocation-layer
      characterization (openac: typed function → passthrough → `system2` with
      one caller-quoted string; tidymedia: `ffm_*` builder → `ffm_args` →
      `run_program` with per-token `shQuote`, plus `system()` escape hatches).
- [ ] T5. Assemble `cairn/references/tidymedia-fit.md` from the synthesis-note
      template with the Provenance block, the snapshot `Extraction:` status, and
      its `INDEX.md` line; run `cairn_validate`.
- [ ] T6. Question gate: present the assessment, record the chosen disposition as
      a D-entry in `cairn/DECISIONS.md`, and add or absorb the ROADMAP bullets
      the Disposition section routes.

## Work log

- 2026-08-08: created by /milestone-plan; promoted from the 2026-08-08 ROADMAP candidate row (user request).
- 2026-08-08: criteria audit ran ([O], fresh context) and returned findings on AC1, AC2, AC3, AC4, AC5, AC7 — AC1 promised evidence `cairn_validate`'s references check does not gather, AC2's "function definition" was undefined for the `ffm`/`ffp` aliases and excluded `require_program()`, AC5's three-filename enumeration was a proxy that dropped tidymedia's colliding `ffm` and its whole invocation layer, AC7 named a "candidate row" this repo's ROADMAP does not have. AC1–AC4 and AC7 fixed pre-gate; AC5 became a gate question. AC6 passed.
- 2026-08-08: plan gate chose assess-and-decide-only over assess-decide-and-adopt because adoption's size is unknowable until the assessment exists; falsified by the assessment concluding adoption is a mechanical one-file change.
- 2026-08-08: plan gate chose the collisions-plus-invocation-layer depth over a full capability sweep of tidymedia's ~86 exports because the sweep's rows are mostly task recipes irrelevant to the dependency question; falsified by a later milestone wanting a tidymedia capability the sweep would have surfaced.
- 2026-08-08: plan chose a two-NAMESPACE intersection over the candidate row's three-filename hint as set C's procedure because the hint enumerated filenames rather than the overlap domain and dropped the one genuinely conflicting name; falsified by the intersection missing a conflict that is not a shared export.
- 2026-08-08: T1 done — set O is 33 members (24 exported, 9 internal, incl. `require_program`), set C is 8; openac exports 48, tidymedia 86. tidymedia pinned at `b99f7e8`; it advanced two commits during M12's planning session, which is why AC1's snapshot `Extraction:` form is the right classifier.
- 2026-08-08: T2 done — 10 of 33 rows name a counterpart. Two dominant findings: the two packages read/write DIFFERENT rappdirs config dirs (`openac` vs `tidymedia`), so depending on tidymedia for discovery strands existing openac users' recorded tool locations; and tidymedia's `find_program` is unexported, so there is no supported way to ask it to resolve openface/opensmile.
- 2026-08-08: T2 observed rather than inferred — `glue("{NULL} -version")` is `character(0)` and `system()` on it errors "non-empty character argument expected", so tidymedia's `ffmpeg()` fails on a missing binary without shell-injecting; and an unquoted space-bearing path fails under `system()`, which `R/ffmpeg.R:28` (unquoted) risks and `R/ffprobe.R:21` (quoted) does not.
- 2026-08-08: T3 done — tidymedia has no CRAN release and no git tags, so a hard dep needs `Remotes: jmgirard/tidymedia`. Framed honestly: openac's CRAN gate is ALREADY closed by `audio.whisper` (`DESCRIPTION:35-36`), so this adds a second independent blocker rather than closing an open gate; the added cost is tidymedia's no-shim clean-break rename policy pre-0.2.0.
- 2026-08-08: T4 done — 6 of the 8 shared exports disagree outright; `ffm` disagrees most sharply (openac: alias of the passthrough; tidymedia: alias of the `ffm_files` job constructor). Invocation-layer verdict: tidymedia quotes once at the boundary (`run_program` + `shQuote`), openac quotes by hand at every call site — the one axis where tidymedia is clearly ahead, and it transfers as an idea without a dependency.
- 2026-08-08: plan chose a committed `references/` synthesis note over keeping the analysis in this milestone file because the CRAN-distribution question and the queued install/discovery candidates will re-read it after this milestone archives; falsified by the decision closing the question with no later reader.

## Decisions

## Review
