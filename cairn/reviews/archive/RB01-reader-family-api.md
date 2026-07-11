# RB01: Tidy-reader family API contract ahead of 1.0 (M03)

- **Date:** 2026-07-11
- **Output required:** write findings to `cairn/reviews/RR01-reader-family-api.md`

You are performing an independent expert review of an R package's API design.
This brief is fully self-contained — do not assume any conversation context.
Read only what this brief directs you to read, answer the numbered questions,
and write your findings to the output path above using the same numbering.

## Background

**openac** provides thin R wrappers around external open-source
affective-computing tools (ffmpeg/ffprobe, OpenFace, openSMILE, and whisper
via the `audio.whisper` package). Its audience is affective-science
researchers who are comfortable in R but not at the command line. The package
is pre-1.0 (`0.0.0.9000`), GitHub-only, with CRAN as the eventual goal.

A **tidy-reader family** is being built: one exported reader per tool that
turns that tool's raw output into a tidy [tibble]. Three now exist:

- `os_read(file)` — openSMILE CSV → wide tibble (M01, shipped).
- `of_read(file)` — OpenFace CSV → wide tibble (M02, shipped).
- `aw_read(x)` — whisper transcription → narrow tibble (M03, **under review
  now, on branch `m03-whisper-reader`**).

`aw_read` is the third reader, and the family is now crystallizing an API
*contract*. Pre-1.0 the API may break freely (D-002), but conventions set here
shape user code and are expensive to change once 1.0 freezes them. The three
readers have **already diverged** along three axes, and no one has decided
whether that divergence is correct or accidental:

1. **Input signature.** `os_read`/`of_read` take a single CSV path (`file`).
   `aw_read` takes a polymorphic `x`: the in-memory transcription object *or*
   a path to its `.rds`/`.csv` output.
2. **Column policy.** `os_read`/`of_read` pass through **every** column as-is.
   `aw_read` **selects** four columns (`segment`, `from`, `to`, `text`) and
   **drops** `segment_offset` (always) and `speaker` (present when diarizing).
3. **Value transformation.** `aw_read` **converts** whisper's
   `"HH:MM:SS.mmm"` timestamp strings to numeric seconds (discarding the
   strings). `os_read` passes values through untouched; `of_read` only strips
   whitespace from column *names*.

The question for you: **what should the reader-family contract be, and which
parts must be settled before 1.0?**

## Materials

Read these (paths are repo-relative):

- **The three readers:**
  - `R/use_opensmile.R` — `os_read()` (function + roxygen; ~line 472 onward,
    section header `# os_read`). Note delimiter auto-detection and
    `check.names = FALSE`.
  - `R/use_openface.R` — `of_read()` (section header `# of_read`, ~line 158).
  - `R/use_whisper.R` — `aw_read()` and its internal helpers `aw_read_data()`
    (dispatch + validation) and `aw_parse_timestamp()` (section header
    `# aw_read`, near end of file).
- **What whisper actually returns:** `cairn/references/audiowhisper.md` — the
  verified structure of `audio.whisper::predict.whisper()` output (class
  `whisper_transcription`; `$data` columns `segment`, `segment_offset`,
  `text`, `from`, `to`, `+speaker` if diarized; timestamp format).
- **aw_read tests (the current contract, made concrete):**
  `tests/testthat/test-whisper-read.R`.
- **Design principles & conventions:** `cairn/DESIGN.md` — esp. "Purpose &
  Scope", "Conventions", GP1 (thin wrappers; parsing outputs is in scope),
  GP2 (batch parity — *capability* not API shape), GP4 (lean dependencies).
- **Decisions:** `cairn/DECISIONS.md` — D-002, D-004, D-005 (see Constraints).
- **Deferred ideas:** `cairn/ROADMAP.md` "Candidates" — note the deferred
  `long=TRUE` pivot, OpenFace feature-block subsetting, and whisper
  `$tokens`/speaker columns.

You do **not** need to run anything (`audio.whisper` is intentionally not
installed). Reason from the source and the reference summary.

## Questions

1. **Input signature.** Is `aw_read`'s polymorphic `x` (object *or* `.rds`
   *or* `.csv`) justified because a whisper result is a live R object (unlike
   the CSV-only tools), or should the family converge on one signature? If
   convergence, in which direction — teach `os_read`/`of_read` to also accept
   objects, make `aw_read` path-only, or a documented split? Recommend the
   contract and say what (if anything) `aw_read` should change now.

2. **Column policy.** Should the family default be **preserve-everything**
   (like `os_read`/`of_read`) or **curate-a-tidy-core** (like `aw_read`)? If
   curation is right, how should presently-dropped-but-useful data
   (`speaker`, and later whisper `$tokens`) be surfaced *without* a post-1.0
   breaking change — e.g. an `extra=`/`columns=` argument, a `speaker=TRUE`
   flag, a separate reader, or keeping the columns and letting users drop
   them? Give a concrete recommended signature.

3. **Value transformation.** Is it right for a function named `*_read` to
   silently transform values (timestamps → seconds), or should readers be
   faithful/lossless and leave derived columns to the user? If transformation
   stays, should the original `"HH:MM:SS.mmm"` strings be retained alongside
   the numeric seconds? State the principle you'd apply and how it generalizes
   to future readers.

4. **Shared shape / tidy contract.** The three outputs are structurally
   heterogeneous (openSMILE: one row per observation, ~thousands of feature
   columns; OpenFace: one row per face per frame; whisper: one row per
   segment, 4 columns). Is a shared cross-reader contract (a common column
   convention, or the deferred `long=TRUE` option) feasible and worth locking
   before 1.0, or are these outputs too heterogeneous to unify? If a minimal
   shared contract is worthwhile, specify it.

5. **1.0 prioritization.** Given D-002 (free breakage pre-1.0), triage the
   choices in Q1–Q4: which are safe to **defer** (cheap to change later) and
   which should be **settled now** because they shape user code and
   expectations most? Rank the "settle now" items.

6. **`aw_read` correctness (secondary).** From the source and
   `cairn/references/audiowhisper.md`, are `aw_parse_timestamp()` and the
   object/`.rds`/`.csv` parity + type-coercion approach sound? Flag any edge
   cases the current tests miss (e.g. segments ≥ 1 hour, `NA`/empty strings,
   the header-only-CSV logical-column case, locale/`OutDec` effects on
   `as.numeric`).

## Constraints

Do not relitigate these; flag disagreement explicitly rather than working
around them silently:

- **D-002** — pre-1.0 the exported API may break freely; no deprecation
  ceremony until 1.0. "We can break it later" is available; the question is
  what *should* be stable, not whether breakage is permitted.
- **D-004** — readers are named `<tool>_read()` (`os_read`, `of_read`,
  `aw_read`). Naming is fixed.
- **D-005** — readers return tibbles. Return type is fixed.
- **GP1** — openac never re-derives signals, but parsing/cleaning/aggregating
  tool *outputs* is in scope; readers are legitimate.
- **GP4** — lean dependencies. Any proposal that adds an Import (e.g.
  `tidyr`/`dplyr` for pivoting) must justify it against this; `tibble` is
  already an Import (D-005).
- **Scope of this review:** a design-*contract* judgement, not a demand to
  rewrite `os_read`/`of_read` now. Recommendations that touch the shipped
  readers should be framed as future milestones/candidates, not as blockers
  on merging M03 — unless you find an outright M03 defect (Q6).

## Output format

In `cairn/reviews/RR01-reader-family-api.md`: answer each question by number
with your reasoning and evidence (cite specific files/lines). List any
additional findings separately under "Beyond the brief". End with concrete
recommendations, each marked **apply** / **consider** / **reject-with-reason**,
and note for each whether it affects M03 now or is a future milestone/candidate.
