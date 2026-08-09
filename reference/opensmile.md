# Low-level access to the opensmile command line interface

Attempt to find and run opensmile with the specified arguments.

## Usage

``` r
opensmile(arg)

os(arg)
```

## Arguments

- arg:

  (character) The arguments to append to the SMILEextract command line
  call, in either of two forms. Give a **character vector** with one CLI
  token per element and each element is quoted for you at the process
  boundary, so a file path may contain spaces or a `$` — and, on
  Windows, a `%TEMP%`-style token, an `&`, a `^` or a `!`. None of those
  are expanded, because openac starts the tool directly rather than
  through a command interpreter. Give a **single string** and it is
  passed through exactly as written, quoting and all, which leaves any
  quoting up to you. Prefer the vector form.

## Value

A character vector containing the output of opensmile. Errors if
opensmile cannot be found.

## References

https://audeering.github.io/opensmile/

## Examples

``` r
if (FALSE) { # \dontrun{
opensmile('-h')
opensmile(c("-C", "my config.conf", "-I", "in.wav"))
} # }
```
