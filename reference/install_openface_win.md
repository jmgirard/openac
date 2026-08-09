# Install openface on Windows

Downloads an openface zip installer, extracts it, and updates the
package's user config files to point to the component executable files.

## Usage

``` r
install_openface_win(download_url = NULL, install_dir = NULL)
```

## Arguments

- download_url:

  A string indicating the location of the openface installation zip
  file. If `NULL`, will default to the version 2.2.0 x64 installer from
  github.

- install_dir:

  A string indicating a directory to install openface to. If `NULL`,
  will default to installing to the user data directory.

## Value

A logical indicating whether the installation was successful.
