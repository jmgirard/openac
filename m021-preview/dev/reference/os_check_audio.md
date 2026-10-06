# Check if an audio file is ready for analysis by openSMILE

Check if an audio file has the proper format for openSMILE, i.e., the
pcm_s16le audio codec and 1 audio channel.

## Usage

``` r
os_check_audio(infile, verbose = FALSE)
```

## Arguments

- infile:

  A required string indicating the filepath of the audio file to check.

- verbose:

  An optional logical indicating whether to print warnings.

## Value

A logical indicating whether `infile` is ready for openSMILE. A file
ffprobe cannot read is not a pass: either query failing returns `FALSE`
with a warning naming the file and the exit status, so an unreadable
file is never reported as merely non-conforming.
