# Install FFmpeg on Windows

Downloads an FFmpeg zip installer, extracts it, and updates the
package's user config files to point to the component executable files.
Note that this function will also install FFprobe.

## Usage

``` r
install_ffmpeg_win(download_url = NULL, install_dir = NULL)
```

## Arguments

- download_url:

  A string indicating the location of the FFmpeg installation zip file.
  If `NULL`, will default to the latest static essentials release from
  gyan.dev.

- install_dir:

  A string indicating a directory to install FFmpeg to. If `NULL`, will
  default to installing to the user data directory.

## Value

A logical indicating whether the installation was successful.
