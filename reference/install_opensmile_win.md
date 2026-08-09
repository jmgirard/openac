# Install openSMILE on Windows

Download a prebuilt openSMILE release for Windows and install it into a
local directory, then register the `SMILExtract.exe` location with
openac.

## Usage

``` r
install_opensmile_win(download_url = NULL, install_dir = NULL)
```

## Arguments

- download_url:

  An optional string giving the URL of the openSMILE Windows archive to
  download. If `NULL`, a pinned official release is used.

- install_dir:

  An optional string giving the directory to install into. If `NULL`, a
  per-user data directory (via
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html))
  is used.

## Value

A logical: `TRUE` on success, `FALSE` if the download or directory
creation failed.

## Examples

``` r
if (FALSE) { # \dontrun{
install_opensmile_win()
} # }
```
