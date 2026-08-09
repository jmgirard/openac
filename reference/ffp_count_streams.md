# Count the streams in a media file

Use ffprobe to count the number of audio and video streams in a media
file.

## Usage

``` r
ffp_count_streams(infile)
```

## Arguments

- infile:

  (string) The filepath to the media file to import.

## Value

A named integer vector with two elements (`Video` and `Audio`)
indicating the number of video and audio streams in `infile`. A file
that cannot be probed — one that does not exist, or one ffprobe rejects
— returns `NA` for both counts with a warning naming it, rather than
raising an error, so a batch records that file and carries on. Two
things still error, because neither is a fact about the file: a missing
ffprobe, which is a problem with the installation, and an `infile` that
is not a single file path. Note that a warning raised by ffprobe itself
reaches you either way — only R's own report of the exit status is
replaced by the message above.
