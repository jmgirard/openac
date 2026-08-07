# audio.whisper — transcription result structure

**Provenance.** Ingested 2026-07-11 by M03 (`85507cf`) from bnosac/audio.whisper's
source, retrieved at
https://raw.githubusercontent.com/bnosac/audio.whisper/master/R/whisper.R —
no shelf item backs it; no PDF, so nothing lives under `pdf/`.
Extraction: read directly off `predict.whisper()` in `R/whisper.R` — the
return-object elements, the `$data` columns, and the `format()` call that
builds `from`/`to` — plus openac's own `R/use_whisper.R` for the writer side.
Last checked against source 2026-07-11.

**Source (primary):** bnosac/audio.whisper, `R/whisper.R` — `predict.whisper()`.
https://raw.githubusercontent.com/bnosac/audio.whisper/master/R/whisper.R
(verified 2026-07-11).

**Return object:** class `"whisper_transcription"`; a list with elements
`n_segments`, `data`, `tokens`, `params`, `timing`.

**`$data` (data.frame), one row per segment — columns:**
`segment` (int), `segment_offset`, `text` (chr, verbatim — often a leading
space), `from` (chr), `to` (chr), and `speaker` (chr, only when
`params$diarize` is TRUE; dropped otherwise).

**Timestamp format (`from`/`to`):** built from millisecond `start`/`end` via
`format(as.POSIXct("1970-01-01 00:00:00", tz = "UTC") + start/1000,
"%H:%M:%OS")` with `options(digits.secs = 3)` → strings `"HH:MM:SS.mmm"`.
So `"00:00:01.500"` = 1.5 s, `"00:01:02.750"` = 62.75 s.

**Writers in openac** (`R/use_whisper.R`): `aw_transcribe()` writes the full
object via `saveRDS()` (`.rds`) and `write.csv(out$data, row.names = FALSE)`
(`.csv`), so the CSV carries every `$data` column (incl. `segment_offset`).

**Traces to:** M03 (`aw_read`) — fixture columns and timestamp oracle.
Per RR01/D-008, `aw_read` keeps `segment`/`from`/`to`/`text` **and `speaker`
when present** (diarization payload), and drops only `segment_offset` (a
redundant re-encoding of `from` in ms).
