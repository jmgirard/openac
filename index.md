# openac

openac provides R wrappers around external, open-source
affective-computing tools — ffmpeg/ffprobe, OpenFace, openSMILE, and
Whisper (via audio.whisper) — so researchers can run them from a single,
consistent R interface. It covers tool discovery, installation, and
configuration; audio/video preparation; single-file and batch extraction
(with parallelism and progress); and reading each tool’s output into a
tidy tibble.

## Installation

You can install the development version of openac from
[GitHub](https://github.com/jmgirard/openac) with:

``` r

# install.packages("pak")
pak::pak("jmgirard/openac")
```

openac calls external command-line tools; install and register the ones
you need
(e.g. [`install_opensmile_mac()`](https://jmgirard.github.io/openac/reference/install_opensmile_mac.md)
/
[`install_opensmile_win()`](https://jmgirard.github.io/openac/reference/install_opensmile_win.md),
or point openac at an existing install with
[`set_opensmile()`](https://jmgirard.github.io/openac/reference/set_program.md)).

## Example

Extract acoustic features from a folder of audio files in parallel, then
read one result into a tidy tibble:

``` r

library(openac)

# Extract eGeMAPS features from every .wav under audio/ into features/.
os_extract_dir(
  indir = "audio",
  inext = "wav",
  aggdir = "features",
  config = "egemaps/v02/eGeMAPSv02"
)

# Read one aggregate output into a one-row-per-file tidy tibble.
feats <- os_read("features/clip01_agg.csv")
```

## Using openac alongside tidymedia

openac and [tidymedia](https://github.com/jmgirard/tidymedia) both wrap
ffmpeg/ffprobe, and both export these eight names:

`ffm`, `ffmpeg`, `ffprobe`, `find_ffmpeg`, `find_ffprobe`, `set_ffmpeg`,
`set_ffprobe`, `set_program`

Attaching both packages masks one set with the other, decided by attach
order, with no warning beyond R’s masking message. Six of the eight
behave differently between the packages, so a masked call can succeed
and do the wrong thing:

- **`ffm` is the sharpest.** In openac it is an alias for
  [`ffmpeg()`](https://jmgirard.github.io/openac/reference/ffmpeg.md)
  and returns the tool’s output. In tidymedia it constructs a command
  object and runs nothing.
- **The `find_*` and `set_*` pairs read and write different
  configuration directories.** A tool location recorded with
  [`openac::set_ffmpeg()`](https://jmgirard.github.io/openac/reference/set_program.md)
  is invisible to `tidymedia::find_ffmpeg()`, and the reverse. Register
  each tool in each package you use it from.
- **[`set_program()`](https://jmgirard.github.io/openac/reference/set_program.md)
  accepts different program names** in each package.

If you need both, qualify the calls —
[`openac::ffmpeg()`](https://jmgirard.github.io/openac/reference/ffmpeg.md),
`tidymedia::ffm()` — rather than relying on attach order. (Observed
against tidymedia at commit `b99f7e8`, 2026-08-08; the two name sets are
compared in full in `cairn/references/tidymedia-fit.md` in this
repository.)

## Code of Conduct

Please note that the openac project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
