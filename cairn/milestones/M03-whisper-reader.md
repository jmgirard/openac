# M03: whisper transcript tidy reader (`aw_read`)

- **Status:** review   <!-- mirror; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- high | normal | low -->
- **Depends on:** M01   <!-- inherits reader-family conventions (naming, wide tibble, tibble Import) -->
- **Branch/PR:** m03-whisper-reader · https://github.com/jmgirard/openac/pull/4

## Goal

Add an exported `aw_read()` that turns an `audio.whisper` transcription
result into a tidy tibble — one row per segment — following the M01 pattern.

## Scope

**In:**
- New exported `aw_read(x)` accepting the object `aw_transcribe()` returns
  (its `$data` holds `segment`, `from`, `to`, `text`) **or** a path to the
  `.rds` (full object) / `.csv` (`out$data`) that `aw_transcribe()` writes
  (R/use_whisper.R:367,378).
- Returns a tibble, one row per segment: `segment` (int), `from`/`to`
  (numeric seconds), `text` (chr, verbatim), plus `speaker` (chr) **when and
  only when** the input carries it (RR01/D-008 — faithful-but-tidy).
- Parse whisper `HH:MM:SS.mmm` timestamps to numeric seconds.
- Empty transcript (no segments) → zero-row tibble with the right columns,
  not an error.
- Errors via `cli::cli_abort()`.

**Out:**
- Token-level output (`$tokens`) → candidate row.
- openSMILE reader → M01; OpenFace reader → M02.

_(RR01/D-008: `speaker` moved from Out → In — dropping it is silent data loss.
`segment_offset` stays dropped as a redundant re-encoding of `from`.)_

## Acceptance criteria

- [x] `aw_read()` exported; returns a tibble with one row per segment.
- [x] `from`/`to` converted to numeric seconds — fixture `"00:00:01.500"`
      yields `1.5`; `text` preserved exactly (incl. leading spaces if present).
- [x] Same tibble from the in-memory result object, its `.rds`, and its
      `.csv` form (all three round-trip to identical output).
- [x] Empty-transcript input → 0-row tibble with columns
      `segment`, `from`, `to`, `text` (no error).
- [x] `cli::cli_abort()` fires on: wrong-type input; missing file for a path
      argument.
- [x] `devtools::test()` green; `devtools::check()` clean — 0E/0W/0N apart
      from the 2 pre-existing vignette warnings (M05's scope; see 2026-07-11
      amendment). Verified with vignettes off, as in M04.

_Added by RR01 ingestion (D-008):_
- [x] (R1) A diarized input yields a `speaker` column; a non-diarized input
      yields no `speaker` column; `segment_offset` still dropped. Fixture
      gains a `diarize = TRUE` variant.
- [x] (R2) Three-form parity holds for adversarial text — a segment whose
      text is literally `"NA"` and an all-numeric-text transcript survive the
      `.csv` path identically to object/`.rds` (`colClasses` pinned).
- [x] (R3) Timestamp oracle covers a ≥ 1 h segment (e.g. `"01:02:03.500"` →
      `3723.5`).
- [x] (R4) DESIGN "Conventions" records the reader-family contract and
      "Function Families" lists `os_read`/`of_read`/`aw_read` (docs).
- [x] (R5) Garbage timestamps warn via `cli`, not a raw `as.numeric` warning.

## Tasks

- [x] Build a fixture mirroring the `audio.whisper` result structure
      (class `whisper_transcription`; `$data` incl. `segment_offset`) via a
      pure-R constructor in the test file, materialised to tempfile
      `.rds`/`.csv` per test (no committed binary; no `audio.whisper` install).
- [x] Write failing tests (`tests/testthat/test-whisper-read.R`): segment
      rows, time parsing, object/rds/csv parity, empty transcript, errors,
      and that `segment_offset` is dropped.
- [x] Implement `aw_read()` in `R/use_whisper.R`: dispatch on input
      (object vs `.rds` vs `.csv`), reach `$data`, parse times, coerce to
      tibble; `cli::cli_abort()` on bad input.
- [x] Roxygen doc + `@examples`; `Rscript -e 'devtools::document()'`.
- [x] `devtools::test()` green (92 pass); `devtools::check()` clean (vignettes
      off) — NEWS entry added for `aw_read()`.

_Added by RR01 ingestion:_
- [x] (R1) Preserve `speaker` conditionally in `aw_read`; update roxygen,
      tests (+ diarized fixture variant), and `cairn/references/audiowhisper.md`.
- [x] (R2) Pin `colClasses` (`text`/`from`/`to`/`speaker`) in the `.csv`
      branch of `aw_read_data()`; add adversarial-text parity tests.
- [x] (R3) Add a ≥ 1 h timestamp case to the oracle test.
- [x] (R4) Write the reader-family contract into DESIGN; list the readers in
      "Function Families".
- [x] (R5) Make garbage-timestamp warnings `cli`-style.
- [x] Re-run `devtools::document()`, `test()`, `check()` (vignettes off).

## Work log
<!-- append-only; one line per entry; absolute dates -->

- 2026-07-11: created by /milestone-plan.
- 2026-07-11: started; branch m03-whisper-reader cut from main.
- 2026-07-11: verified `audio.whisper` `predict.whisper()` output against its
  source (bnosac/audio.whisper `R/whisper.R`). `$data` columns are `segment`,
  `segment_offset`, `text`, `from`, `to` (+ `speaker` if diarized) — the plan
  omitted `segment_offset`. `from`/`to` = `format(POSIXct + start/1000,
  "%H:%M:%OS")` → `"HH:MM:SS.mmm"`. Reference note added.
- 2026-07-11: amendment (minor) — fixture built in a testthat helper (pure-R
  constructor incl. `segment_offset`, class `whisper_transcription`), written
  to tempfile `.rds`/`.csv` per test, rather than committing a binary `.rds`
  blob. More transparent oracle; also tests that `aw_read` drops the extra
  `segment_offset` column.
- 2026-07-11: amendment (minor) — final criterion "check() clean" scoped to
  "clean apart from the 2 vignette warnings (M05)". Forced by the M04/M05
  vignette split that post-dates M03's planning; mirrors M04's user-approved
  treatment. M03 itself adds 0 new check findings.
- 2026-07-11: implemented `aw_read()` (+ internal `aw_read_data`,
  `aw_parse_timestamp`); 25 new tests, all green; suite 92 pass;
  `check()` (vignettes off) 0E/0W/0N. Status → review.
- 2026-07-11: blocked on RB01 (tidy-reader family API contract, Fable
  escalation). Brief created on this branch (not main) so M03's status and the
  RR-ingested decisions stay coherent while M03 is unmerged. Held from merge
  until the RR is ingested — the RR may recommend changing `aw_read`
  (columns/timestamps).
- 2026-07-11: ingested RR01 → D-008 (reader-family contract). Apply in M03:
  R1 preserve `speaker`, R2 CSV `colClasses` parity fix (a real defect —
  `"NA"`/numeric text corrupted via `.csv`), R3 ≥1h test, R4 DESIGN contract,
  R5 cli timestamp warnings. R6 (accept bare `$data`) + R7 (multi-file `id`
  idiom) → candidates. R8–R12 rejected (see RR01 table). RB01/RR01 archived;
  status → in-progress.
- 2026-07-11: applied R1–R5. R2 note — the RR's `colClasses` one-liner was
  *insufficient*; verified empirically that `"NA"` text still coerced to `NA`,
  so the fix also needs `na.strings = character(0)` (new `aw_read_csv()`
  helper). Suite 108 pass; `check()` (vignettes off) 0E/0W/0N. Status → review.
- 2026-07-11: review — PR #4; fresh evidence re-gathered; independent Opus
  review clean (2 nits applied, 1 rejected). Awaiting merge approval.

## Decisions
<!-- milestone-local; promote cross-cutting ones to cairn/DECISIONS.md -->

- Inherits D-004 (naming) and D-005 (`tibble` Import) from M01.
- D-008 (2026-07-11, cross-cutting): tidy-reader family API contract, from
  RR01. Drives the R1–R5 tasks above. Supersedes M03's earlier scope note
  that deferred `speaker` to a candidate.

## Review

_Reviewed 2026-07-11 · PR #4 · branch `m03-whisper-reader`._

**Acceptance criteria (fresh evidence):** all met. `test()` 108 pass;
`check()` (vignettes off) 0E/0W/0N apart from the 2 named vignette warnings
(M05). Independently re-verified: hour-scale `01:02:03.500`→`3723.5`;
three-form (object/`.rds`/`.csv`) parity for plain, diarized, and
adversarial (`"NA"`/numeric) text; empty→0-row/4-col; both `cli_abort`
branches fire. R1–R5 (speaker, CSV parity fix, ≥1h test, DESIGN contract,
cli warnings) all present and tested.

**Consistency gate:** `document()` idempotent ✓; README.Rmd/.md untouched &
in sync ✓; no pkgdown site ✓; NEWS entry present (`aw_read`, no milestone
numbers) ✓; no new top-level tracked files ✓.

**Independent Opus review (fresh context):** clean — no blockers, no
should-fix. Empirically stress-tested parity (embedded quotes/commas/
newlines, actual-NA, empty string) and confirmed correct. Three NITs: (1)
document the deliberate `na.strings` trade → **applied** (comment); (2) error
tests assert message not class → **applied** (added `class = "rlang_error"`);
(3) garbage in both `from` and `to` emits two `cli_warn`s → **rejected**
(reviewer agreed it is harmless and arguably clearer than one merged warning).

**Provenance:** design settled via Fable escalation RB01/RR01 → D-008
(archived under `cairn/reviews/archive/`).
