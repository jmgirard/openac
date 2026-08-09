# Set the location of a dependency program

Set the location of a dependency program

## Usage

``` r
set_program(program, location)

set_ffmpeg(location)

set_ffprobe(location)

set_openface(location)

set_opensmile(location)
```

## Arguments

- program:

  A string indicating which program to set the location for.

- location:

  A string containing the location of the program.

## Value

Invisibly, `NULL`. Called for its side effect: recording `location` in
the user config directory, where
[`find_program()`](https://jmgirard.github.io/openac/reference/find_program.md)
reads it.
