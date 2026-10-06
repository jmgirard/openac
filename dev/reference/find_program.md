# Find the location of a dependency program

Returns the location of the requested program as a string.

## Usage

``` r
find_program(program)

find_ffmpeg()

find_ffprobe()

find_openface()

find_opensmile()
```

## Arguments

- program:

  (character) Which program to find? Can be either "ffmpeg", "ffprobe",
  "openface", or "opensmile"

## Value

An absolute path to the program as a string, or `NULL` (with a warning)
if the program could not be found.
