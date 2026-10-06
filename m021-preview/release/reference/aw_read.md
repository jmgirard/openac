# Read a Whisper transcription into a tidy tibble

Turn the result of
[`aw_transcribe()`](https://jmgirard.github.io/openac/reference/aw_transcribe.md)
into a tidy [tibble](https://tibble.tidyverse.org/reference/tibble.html)
with one row per transcript segment. Accepts the transcription object
itself, or a string path to the `.rds` (full object) or `.csv` (`$data`)
file that
[`aw_transcribe()`](https://jmgirard.github.io/openac/reference/aw_transcribe.md)
writes — all three forms yield identical output.

## Usage

``` r
aw_read(x)
```

## Arguments

- x:

  A Whisper transcription: the object returned by
  [`aw_transcribe()`](https://jmgirard.github.io/openac/reference/aw_transcribe.md),
  or a string path to its `.rds` or `.csv` output.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per segment and columns `segment` (integer), `from`/`to` (numeric
seconds), `text` (character), and — for diarized transcriptions —
`speaker` (character).

## Details

Whisper's `HH:MM:SS.mmm` start/end timestamps are parsed to numeric
seconds. Segment text is preserved verbatim (Whisper often emits a
leading space). A `speaker` column is kept when (and only when) the
transcription carries one — i.e. it was produced with diarization. The
redundant `segment_offset` (the start time re-encoded in milliseconds)
is dropped, as is token-level output (out of scope).

## See also

[`aw_transcribe()`](https://jmgirard.github.io/openac/reference/aw_transcribe.md),
which produces the input.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- aw_transcribe("audio.wav", model = aw_get_model("tiny"))
segments <- aw_read(res)
} # }
```
