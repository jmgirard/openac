# Install openSMILE on macOS

Download a prebuilt openSMILE release for macOS and install it into a
local directory, then register the `SMILExtract` location with openac.

## Usage

``` r
install_opensmile_mac(
  download_url = NULL,
  install_dir = NULL,
  arch = c("armv8", "x86_64")
)
```

## Arguments

- download_url:

  An optional string giving the URL of the openSMILE macOS archive to
  download. If `NULL`, a pinned official release matching `arch` is
  used.

- install_dir:

  An optional string giving the directory to install into. If `NULL`, a
  per-user data directory (via
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html))
  is used.

- arch:

  The CPU architecture to install for, either `"armv8"` (Apple silicon)
  or `"x86_64"` (Intel). Ignored when `download_url` is supplied.

## Value

A logical: `TRUE` on success, `FALSE` if the download or directory
creation failed.

## Examples

``` r
if (FALSE) { # \dontrun{
install_opensmile_mac()
} # }
```
