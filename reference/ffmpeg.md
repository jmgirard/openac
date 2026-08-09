# Low-level access to the ffmpeg command line interface

Attempt to find and run ffmpeg with the specified arguments.

## Usage

``` r
ffmpeg(arg)

ffm(arg)
```

## Arguments

- arg:

  (character) The arguments to append to the ffmpeg command line call,
  in either of two forms. Give a **character vector** with one CLI token
  per element and each element is quoted for you at the process
  boundary, so a file path may contain spaces or a `$` — and, on
  Windows, a `%TEMP%`-style token, an `&`, a `^` or a `!`. None of those
  are expanded, because openac starts the tool directly rather than
  through a command interpreter. Give a **single string** and it is
  passed through exactly as written, quoting and all, which leaves any
  quoting up to you. Prefer the vector form.

## Value

A character vector containing the output of ffmpeg. Errors if ffmpeg
cannot be found.

## References

https://ffmpeg.org/ffmpeg.html

## Examples

``` r
if (FALSE) { # \dontrun{
ffmpeg('-version')
ffmpeg(c("-i", "my video.mp4", "-c:a", "pcm_s16le", "my audio.wav"))
} # }
```
