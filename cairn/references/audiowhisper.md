# audio.whisper — transcription result structure

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

**Traces to:** M03 (`aw_read`) — fixture columns, timestamp oracle, and the
requirement that `aw_read` selects `segment`/`from`/`to`/`text` and drops
`segment_offset`/`speaker`.
