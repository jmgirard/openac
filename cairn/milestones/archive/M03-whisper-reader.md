# M03: whisper transcript tidy reader (`aw_read`) — done

**Status:** done · **PR:** #4 · merged 2026-07-11 · Priority: normal · Depends: M01

## Goal
Exported `aw_read()` turning an `audio.whisper` transcription into a tidy
tibble, one row per segment (third reader after `os_read`/`of_read`).

## Outcome
`aw_read(x)` accepts the transcription object **or** its `.rds`/`.csv` path —
all three yield identical output. Columns `segment` (int), `from`/`to` (numeric
seconds from `"HH:MM:SS.mmm"`), `text` (chr verbatim), `speaker` (chr) when
diarized; `segment_offset` dropped. Empty → 0-row; `cli::cli_abort()` on bad
input. Helpers `aw_read_data`/`aw_read_csv`/`aw_parse_timestamp`. `test()` 108
pass; `check()` (vignettes off) 0E/0W/0N (2 vignette warnings → M05).

## Key decisions / provenance
- **D-008** — tidy-reader family API contract (input-form equivalence,
  faithful-but-tidy columns, lossless type-parsing, time in numeric seconds);
  in DESIGN "Conventions"/"Function Families". `$data` structure verified vs
  `audio.whisper` source (`cairn/references/audiowhisper.md`).
- Design settled via Fable RB01/RR01 (archived): caught a `.csv` parity
  **defect** (`"NA"`/numeric text) + silent `speaker` loss, both fixed (the
  `colClasses` fix also needed `na.strings = character(0)`). Opus review clean.
