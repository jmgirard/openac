# Extract opensmile features

Extract openSMILE acoustic features from an audio file based on a config
file. Lower level descriptors (LLDs) will be calculated per frame and
then summarized into an aggregate (AGG) file.

## Usage

``` r
os_extract(
  infile,
  wavfile = NULL,
  aggfile = NULL,
  lldfile = NULL,
  config = "misc/emo_large",
  ...
)
```

## Arguments

- infile:

  (character) What is the filepath for the input file to be analyzed?
  The proper format can be created by
  [`os_prep_audio()`](https://jmgirard.github.io/openac/reference/os_prep_audio.md).

- wavfile:

  (character, default=NULL) Either NULL or a string indicating the path
  to save the prepared version of `infile` to (must end with '.wav'). If
  NULL, a temporary file will be created and later discarded.

- aggfile:

  (character, default=NULL) What is the filepath to write the AGG output
  to? If `NULL`, the AGG output will not be saved. Note that either
  `aggfile` or `lldfile` (or both) must be non-NULL.

- lldfile:

  (character, default=NULL) What is the filepath to write the LLD output
  to? If `NULL`, the LLD output will not be saved. Note that either
  `aggfile` or `lldfile` (or both) must be non-NULL.

- config:

  (character, default="misc/emo_large") Which configuration file should
  be used to analyze `infile`? A list of available config files can be
  generated using
  [`os_list_configs()`](https://jmgirard.github.io/openac/reference/os_list_configs.md).

- ...:

  Arguments passed on to
  [`os_prep_audio`](https://jmgirard.github.io/openac/reference/os_prep_audio.md)

  `stream`

  : (numeric, default=0) The index of the audio stream to extract
    (ffmpeg uses zero-indexing so 0 is the first stream).

  `overwrite`

  : Should outfile be overwritten if it already exists? It will be
    skipped otherwise, silently for a direct call. In a batch the row
    depends on whose job the preparing is: under
    [`os_prep_audio_dir()`](https://jmgirard.github.io/openac/reference/os_prep_audio_dir.md)
    it is the whole job, so the row reads `"skipped"`; under
    [`os_extract_dir()`](https://jmgirard.github.io/openac/reference/os_extract_dir.md)
    the existing file is reused and openSMILE still runs, so the row
    reads `"ok"`. Defaults to TRUE.

## Value

A character vector including opensmile output. Errors, naming the file,
if openSMILE exits non-zero.
