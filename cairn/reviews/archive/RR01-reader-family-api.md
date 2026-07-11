# RR01: Tidy-reader family API contract ahead of 1.0 (M03)

- **Date:** 2026-07-11
- **Brief:** `cairn/reviews/RB01-reader-family-api.md`
- **Reviewer:** independent Fable-level review (no session context)
- **Materials read:** `R/use_opensmile.R` (`os_read`, l. 504–531),
  `R/use_openface.R` (`of_read`, l. 184–205), `R/use_whisper.R` (`aw_read`
  l. 539–547, `aw_read_data` l. 551–589, `aw_parse_timestamp` l. 593–599),
  `cairn/references/audiowhisper.md`, `tests/testthat/test-whisper-read.R`,
  `cairn/DESIGN.md`, `cairn/DECISIONS.md` (D-002/D-004/D-005/D-006),
  `cairn/ROADMAP.md`. Behavioral claims about `read.csv`/`as.numeric`
  edge cases were verified by running R directly (noted inline).

---

## 1. Input signature

**Verdict: the divergence is correct, not accidental — keep it, but write it
down as a single principle so it stops looking like drift.**

The three tools differ in what their *native output artifact* is:

- openSMILE and OpenFace are CLI programs whose only output is a file on
  disk. There is no in-memory object for `os_read`/`of_read` to accept;
  `os_extract()`/`of_extract()` return command output text, not data
  (`R/use_opensmile.R`, `R/use_openface.R`). A `file` path is the complete
  set of possible inputs.
- whisper (via `audio.whisper`) is R-native: `aw_transcribe()` *returns* the
  live `whisper_transcription` object and only *optionally* writes `.rds`/
  `.csv` sidecars (`R/use_whisper.R` l. 367–380, `rdsfile`/`csvfile` default
  `NULL`). If `aw_read` were path-only, the most common interactive flow —
  `res <- aw_transcribe(...); aw_read(res)` — would force a pointless
  save-to-disk round trip, which is hostile to exactly the audience DESIGN
  names (researchers "comfortable in R but not at the command line").

So the family contract should be stated as one rule, not three cases:

> **A reader accepts every form in which its tool's output natively exists:
> a file path for file-producing tools; the in-memory object *plus* the
> serialized files the wrapper itself writes, for R-level tools. All accepted
> forms yield identical output.**

Under that rule `os_read(file)`/`of_read(file)` and `aw_read(x)` are the
*same* contract instantiated on different tools. The parameter-name split is
also principled and worth codifying: `file` when the input can only be a
path; `x` when it is polymorphic. Teaching `os_read`/`of_read` to accept
objects is a non-starter (no object exists); making `aw_read` path-only is a
strict ergonomics loss for zero consistency gain (the signatures would still
differ in docs and error behavior).

**What `aw_read` should change now: nothing.** The dispatch in
`aw_read_data()` (l. 551–589) already enforces the "identical output" clause
(tested at `test-whisper-read.R` l. 72–83 and 93–102). The only action is
documentation: add the rule above to DESIGN "Conventions" so reader #4
inherits it deliberately.

## 2. Column policy

**Verdict: the family default should be *faithful-but-tidy* — pass through
all data-bearing columns; drop only columns that are redundant re-encodings
of retained ones. `aw_read`'s dropping of `segment_offset` passes that test;
its dropping of `speaker` fails it and should be fixed in M03.**

Reasoning:

- `os_read`/`of_read` are preserve-everything and that is right for them:
  every openSMILE feature and OpenFace block is data a researcher may need,
  and the reader cannot know which (`R/use_opensmile.R` l. 520–530,
  `R/use_openface.R` l. 198–204).
- `segment_offset` is the segment start in milliseconds
  (`cairn/references/audiowhisper.md` l. 10–13) — i.e., the same information
  as `from` in a different unit. Dropping a redundant re-encoding is
  curation a reader may do; nothing is lost.
- `speaker` is **not** redundant — it is the *primary payload of
  diarization*. A user who runs whisper with `diarize = TRUE` and then calls
  `aw_read()` (l. 539–547 hard-codes four columns) silently loses the very
  column they diarized to obtain. That is data loss, the one thing a reader
  must never silently do. The current behavior is documented (roxygen
  l. 523–526) and tested (`test-whisper-read.R` l. 66–70), but documented
  data loss is still data loss. Note the fixture never exercises a diarized
  result, which is how this slipped through.

**Recommended concrete signature:** keep `aw_read(x)` with *no* new
arguments. Output columns: `segment`, `from`, `to`, `text`, **and `speaker`
(character) when and only when the input carries it**. The conditional
column mirrors the tool's own conditional output — no `speaker = TRUE` flag
is needed, and the flag would be worse (a user must already know whether
they diarized; the reader can see it). This works identically across all
three input forms because `aw_transcribe()`'s CSV carries every `$data`
column (`audiowhisper.md` l. 20–22).

**How to surface future extras without post-1.0 breakage:** adding optional
arguments and adding columns are both non-breaking moves in practice
(scripts that `select()` keep working; scripts that assume "exactly 4
columns by position" are already fragile). So the safe 1.0 posture is:
ship the minimal faithful core now, and reserve these *additive* paths:

- `speaker` — include now (see above); zero future cost.
- whisper `$tokens` — different granularity (one row per token, not per
  segment), so it does not belong in the same tibble. Surface later as
  either a separate reader (`aw_read_tokens(x)`) or a `tokens =` list-column
  option; both are additive. Stays a ROADMAP candidate (already listed,
  `ROADMAP.md` l. 21).
- OpenFace feature-block subsetting — additive `columns =`/`blocks =`
  argument on `of_read` later; defer (already a candidate).

What must be *avoided* before 1.0 is the opposite move: shipping readers
that drop data and later re-adding it, training early users to distrust the
readers or to bypass them with `read.csv` — which defeats the family's
purpose.

## 3. Value transformation

**Verdict: keep the timestamp conversion; do not retain the original
strings. The principle: a reader may perform *lossless representation
changes into natural R types*, and nothing else.**

`"HH:MM:SS.mmm"` → numeric seconds is invertible (millisecond precision is
preserved; `aw_parse_timestamp`, l. 593–599, verified: `"01:02:03.500"` →
`3723.5`). It is exactly the kind of "parsing at import" that tidy readers
(`readr`, `haven`) already do for dates and labelled values, and GP1
explicitly puts "parsing, cleaning" of tool outputs in scope
(`DESIGN.md` l. 75–78). Keeping the strings alongside would add two columns
that carry zero additional information and invite unit confusion.

The stronger argument is cross-reader coherence: openSMILE's `frameTime` and
OpenFace's `timestamp` are already numeric seconds in the tools' own output.
Converting whisper's strings makes **"time is numeric seconds" a family
invariant** — arguably the single most valuable piece of shared contract
available (see Q4). Leaving strings would be the *inconsistent* choice.

Generalized principle for future readers, in increasing order of what is
allowed:

1. **Names:** mechanical cleanup only (e.g., `of_read`'s `trimws`,
   `R/use_openface.R` l. 203); never rename to taste — verbatim names keep
   outputs greppable against the tool's documentation.
2. **Types:** parse encoded scalars to natural R types when the encoding is
   unambiguous and invertible (timestamp strings → seconds; "1"/"0" flags →
   integer as OpenFace already emits them). This is what `*_read` means.
3. **Never:** unit conversions that lose precision, row filtering,
   aggregation, or derived quantities (e.g., segment durations) — those are
   analysis, and GP1's "no re-deriving signals" spirit puts them on the
   user's side of the line.

`os_read` (values untouched) and `of_read` (names only) already sit inside
this principle; no retrofit needed.

## 4. Shared shape / tidy contract

**Verdict: full structural unification is neither feasible nor desirable;
a minimal shared contract is both, and is cheap to lock now.**

The outputs answer different questions at different granularities (one row
per observation × ~6500 features; one row per face-frame; one row per
segment). Forcing them into one schema (or one mandatory long format) would
manufacture sameness the data doesn't have. The deferred `long = TRUE` also
runs into GP4: a proper pivot wants `tidyr`, a new Import, to save the user
one line of their own `pivot_longer()` — poor trade, and *adding* `long =`
later is non-breaking anyway. Defer it (stays a candidate).

The minimal contract worth locking (items 1–2 are already decided; 3–6 are
implicitly true today and just need writing down in DESIGN):

1. **Return type:** tibble (D-005, fixed).
2. **Naming:** `<tool>_read()` (D-004, fixed).
3. **Grain:** one row per the tool's natural observation unit, stated in the
   first sentence of each reader's roxygen (all three already do this).
4. **Time in numeric seconds** wherever a time column exists
   (`frameTime`, `timestamp`, `from`/`to` — all already comply given Q3).
5. **Column names verbatim** from the tool, minus mechanical whitespace
   cleanup; no renaming, no `check.names` mangling (all comply:
   `use_opensmile.R` l. 527, `use_openface.R` l. 200–203,
   `use_whisper.R` l. 566).
6. **Faithful-but-tidy columns** (Q2): all data-bearing columns pass
   through; only documented redundant re-encodings may be dropped.
7. **Input-form equivalence** (Q1): every accepted input form yields
   identical output.

This is a *conventions* contract, not a schema — it is what lets a user who
has learned one reader predict the next one, without pretending a
transcript is a face track.

## 5. 1.0 prioritization

**Settle now, ranked:**

1. **Column policy / `speaker` (Q2).** The only item with silent data loss,
   and the one that most shapes user trust and downstream code. Fix
   `aw_read` in M03 (it is unshipped — cheapest moment it will ever have)
   and record the faithful-but-tidy rule.
2. **Value-transformation principle / time-in-seconds invariant (Q3+Q4.4).**
   Numeric timestamps become load-bearing in analysis scripts immediately;
   flipping representation later is the most script-breaking kind of change
   (type change, not name change). Costs one DESIGN paragraph now.
3. **Input-signature rule (Q1) + the rest of the minimal contract (Q4).**
   Zero code, one DESIGN "Conventions" addition. Do it before reader #4
   exists, because conventions are set by the third example and copied by
   the fourth.

**Safe to defer** (all additive later, per D-002 not even needing ceremony):
`long = TRUE` pivot; `$tokens` access; OpenFace feature-block subsetting;
any `*_read_dir`/vectorized batch reading (see "Beyond the brief");
retrofits to shipped `os_read`/`of_read` (none are needed by this review).

## 6. `aw_read` correctness (secondary)

Overall: **sound**. `aw_parse_timestamp()` is correct for the format whisper
actually emits, the three-form parity is genuinely enforced by construction
(one `aw_read_data()` funnel) and by tests, and the empty-transcript
`header-only CSV → logical columns` trap is already neutralized by the
explicit `as.integer`/`as.character` coercions in `aw_read()` (l. 542–545)
and covered by tests (`test-whisper-read.R` l. 85–102). Specific findings
(all verified by execution unless noted):

- **≥ 1 hour untested.** `aw_parse_timestamp("01:02:03.500")` → `3723.5`
  (correct; the `60^rev(seq_along(nums)-1)` positional weighting is right),
  but the test fixture's largest timestamp is `00:01:02.750`
  (`test-whisper-read.R` l. 26). Hour-scale recordings are routine in
  affective science. Add a ≥ 1 h case to the oracle test. *(Test gap, not a
  bug.)*
- **CSV path can corrupt `text` via `read.csv` type inference.** Verified:
  a segment whose text is literally `"NA"` becomes `NA` after the CSV
  round-trip (quoting does **not** protect it — `na.strings` applies after
  unquoting), and a transcript whose *every* text is numeric-looking
  (e.g. `" 42"`) comes back as an integer column, losing the leading space
  and the character type. Both break the documented "all three forms yield
  identical output" contract (roxygen l. 517–521), object/`.rds` unaffected.
  Fix in `aw_read_data()` l. 566 by pinning classes, e.g.
  `read.csv(x, colClasses = c(text = "character", from = "character",
  to = "character", speaker = "character"), check.names = FALSE)` — note a
  named `colClasses` referencing absent columns *works* but emits a warning
  ("not all columns named in 'colClasses' exist"), so either wrap in
  `suppressWarnings()` or intersect the names against the header first.
  Low real-world probability, one-line fix, and it also future-proofs the
  `speaker` column (Q2). **This is the one genuine M03 defect found.**
- **Malformed timestamps are inconsistently silent.** `""` and `NA` →
  `NA_real_` silently (by design, l. 595); but garbage like `"abc"` → `NA`
  with a raw base-R warning `"NAs introduced by coercion"` escaping from
  `as.numeric` inside `vapply` — off-brand for a package whose convention is
  `cli` conditions for a non-technical audience (`DESIGN.md` Conventions).
  Consider `suppressWarnings()` inside the helper plus a single
  `cli_warn()` when the parsed result contains new `NA`s. Minor.
- **Locale / `OutDec`: safe.** Verified `as.numeric("1.5")` returns `1.5`
  under `options(OutDec = ",")` — `OutDec` affects output formatting only,
  and `as.numeric` parses C-locale numerals regardless. The CSV path is
  also symmetric: `aw_transcribe()` writes with `write.csv` (dec = `.`) and
  `aw_read_data()` reads with `read.csv` defaults (dec = `.`). No action.
- **> 24 h recordings (upstream, unfixable here).** `audio.whisper` builds
  the strings via `format(POSIXct, "%H:%M:%OS")` (`audiowhisper.md`
  l. 15–17), so hour 25 wraps to `"01:..."` *upstream*; `aw_parse_timestamp`
  itself would handle `"25:00:00.000"` → `90000` fine (verified). Not an
  `aw_read` bug; at most worth a sentence in the roxygen if anyone ever hits
  it. No action.
- **Dispatch leniency is acceptable.** `aw_read_data()` duck-types any list
  with a `$data` element (l. 572) rather than checking
  `class == "whisper_transcription"` — reasonable (survives upstream class
  renames; the column check at l. 580–588 is the real guard). Passing the
  bare `$data` data.frame errors with a clear message; acceptable, though
  accepting a data.frame that already has the four columns would be a
  harmless convenience. Optional.

## Beyond the brief

- **DESIGN.md is stale about the readers.** "Function Families" (l. 40–48)
  lists neither `os_read` nor `of_read` (both shipped, M01/M02), and
  "Purpose & Scope" (l. 22–24) still calls tidy readers "mostly unbuilt,
  `os_fix_csv` is the seed". When M03 merges, the reader family deserves its
  own bullet — and it is the natural place to record the Q1/Q3/Q4 contract.
- **GP2 (batch parity) vs the readers.** Every reader is single-file with no
  batch counterpart. GP2 says the principle is capability, not API shape
  (`DESIGN.md` l. 79–84) — and `purrr::map_dfr(files, os_read)` is a
  one-liner — but for the stated audience a documented idiom or an `id`
  column convention for row-binding multiple files may eventually matter.
  Suggest a ROADMAP candidate line; not an M03 concern.
- **The reference doc encodes the contested behavior as a requirement.**
  `cairn/references/audiowhisper.md` l. 25–26 states "the requirement that
  `aw_read` … drops `segment_offset`/`speaker`". If the Q2 recommendation is
  accepted, update that sentence, the roxygen (l. 523–526), and the drop
  test (l. 66–70) together, and extend `make_aw_result()` with a
  `diarize = TRUE` variant so the speaker path is actually exercised.

## Recommendations

| # | Recommendation | Marking | When |
|---|---|---|---|
| R1 | Preserve `speaker` (conditionally, when present) in `aw_read`; keep dropping `segment_offset`; update roxygen, tests (add a diarized fixture), and `audiowhisper.md` | **apply** | M03 now |
| R2 | Pin `colClasses` for `text`/`from`/`to`/`speaker` in the `.csv` branch of `aw_read_data()` to restore exact three-form parity (`"NA"`-text and numeric-text cases) | **apply** | M03 now |
| R3 | Add a ≥ 1 hour timestamp case to the `aw_parse_timestamp` oracle test | **apply** | M03 now |
| R4 | Record the reader-family contract in DESIGN "Conventions": input-form rule (Q1), faithful-but-tidy column rule (Q2), lossless-type-parsing rule + time-in-numeric-seconds invariant (Q3/Q4), grain-documented-per-reader, verbatim names; and add the readers to "Function Families" | **apply** | M03 review or immediately after merge (docs only) |
| R5 | Make garbage-timestamp warnings `cli`-style (suppress `as.numeric`'s base warning, emit one `cli_warn` on new `NA`s) | **consider** | M03 if trivial, else opportunistic (per existing Conventions migration policy) |
| R6 | Accept a bare `$data`-shaped data.frame in `aw_read` | **consider** | future candidate; marginal value |
| R7 | Add ROADMAP candidate: multi-file reading idiom / `id`-column convention for readers (GP2 capability) | **consider** | future candidate |
| R8 | Teach `os_read`/`of_read` to accept in-memory objects | **reject-with-reason** — no such object exists for CLI tools; the polymorphism rule (R4) makes the split principled, not divergent | — |
| R9 | Make `aw_read` path-only for signature uniformity | **reject-with-reason** — forces a disk round-trip on the primary interactive flow for zero real consistency gain | — |
| R10 | Retain original `"HH:MM:SS.mmm"` strings alongside numeric seconds | **reject-with-reason** — conversion is invertible; duplicate columns add clutter and break the time-in-seconds family invariant | — |
| R11 | Lock a `long = TRUE` cross-reader shape before 1.0 | **reject-with-reason** — outputs are legitimately heterogeneous; a pivot argument is additive later and would pressure GP4 (tidyr Import) now | — |
| R12 | `speaker = TRUE`/`extra =` argument instead of conditional column | **reject-with-reason** — the reader can see whether diarization happened; a flag pushes tool-state bookkeeping onto the user and adds API surface for nothing | — |

**Net effect on M03:** R1–R3 are small, test-covered changes to the branch
under review (R2 fixes an outright parity defect; R1 fixes silent data
loss); none of the remaining items block merge.
