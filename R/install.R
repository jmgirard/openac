
# find_program() ---------------------------------------------------------------

#' Find the location of a dependency program
#'
#' Returns the location of the requested program as a string.
#'
#' @param program A string indicating which program to find
#' @return Either a string indicating whether the requested program was found or
#'   `NULL` if the program could not be found.
find_program <- function(
    program = c("ffmpeg", "ffprobe", "ffplay", "openface", "opensmile")) {

  # Validate arguments
  program <- match.arg(program)

  # First, look for program in path
  location <- Sys.which(program)

  if (location == "") {
    # If program not found, look for a user config file
    config <- file.path(
      rappdirs::user_config_dir("openac", "R"),
      paste0(program, "_location.txt")
    )
    # If a user config file exists, read it in
    if (file.exists(config)) {
      location <- readLines(config)
      # Verify that the location in the user config file is valid
      if (Sys.which(location) == "") {
        warning(
          paste0(
            program,
            " was set as being at ",
            location,
            " but this file does not seem to exist anymore."
          )
        )
        location <- NULL
      }
    } else {
      # If config file not found, return NULL value and warning
      location <- NULL
      warning(
        paste0(
          "Failed to find ",
          program,
          ". Check that it is installed and, if necessary, ",
          "use the set_program() function."
        )
      )
    }
  }

  tools::file_path_as_absolute(location)
}

# find_ffmpeg() ----------------------------------------------------------------

#' @rdname find_program
#' @export
find_ffmpeg <- function() {
  find_program("ffmpeg")
}

# find_ffprobe() ---------------------------------------------------------------

#' @rdname find_program
#' @export
find_ffprobe <- function() {
  find_program("ffprobe")
}

# find_ffplay() ----------------------------------------------------------------

#' @rdname find_program
#' @export
find_ffplay <- function() {
  find_program("ffplay")
}

# find_openface() --------------------------------------------------------------

#' @rdname find_program
#' @export
find_ffplay <- function() {
  find_program("openface")
}

# find_openface() -------------------------------------------------------------

#' @rdname find_program
#' @export
find_openface <- function() {
  find_program("openface")
}

# find_opensmile() -------------------------------------------------------------

#' @rdname find_program
#' @export
find_opensmile <- function() {
  find_program("opensmile")
}

# set_program() ----------------------------------------------------------------

#' Set the location of a dependency program
#'
#' @param program A string indicating which program to set the location for.
#' @param location A string containing the location of the program.
#' @return A logical indicating whether the program location was set properly.
#'
#' @export
set_program <- function(
    program = c("ffmpeg", "ffprobe", "ffplay", "openface", "opensmile"),
    location) {

  # Validate arguments
  program <- match.arg(program)
  stopifnot(is.character(location), length(location) == 1)
  stopifnot(Sys.which(location) != "")

  # Find where to save user configuration data
  config_dir <- rappdirs::user_config_dir("openac", "R")
  config_file <- file.path(config_dir, paste0(program, "_location.txt"))

  # Create configuration directory if needed
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)

  # Save location to user configuration file
  writeLines(location, config_file)
}

# set_ffmpeg() ------------------------------------------------------------

#' @rdname set_program
#' @export
set_ffmpeg <- function(location) {
  set_program("ffmpeg", location)
}

#' @rdname set_program
#' @export
set_ffprobe <- function(location) {
  set_program("ffprobe", location)
}

#' @rdname set_program
#' @export
set_ffplay <- function(location) {
  set_program("ffplay", location)
}

#' @rdname set_program
#' @export
set_openface <- function(location) {
  set_program("openface", location)
}

#' @rdname set_program
#' @export
set_opensmile <- function(location) {
  set_program("opensmile", location)
}

# install_ffmpeg_win() --------------------------------------------------------

#' Install FFmpeg on Windows
#'
#' Downloads an FFmpeg zip installer, extracts it, and updates the package's
#' user config files to point to the component executable files.
#'
#' @param download_url A string indicating the location of the FFmpeg
#'   installation zip file. If `NULL`, will default to the latest static
#'   essentials release from gyan.dev.
#' @param install_dir A string indicating a directory to install FFmpeg to. If
#'   `NULL`, will default to installing to the user data directory.
#' @return A logical indicating whether the installation was successful.
#' @export
install_ffmpeg_win <- function(download_url = NULL, install_dir = NULL) {

  if (is.null(download_url)) {
    download_url <-
      "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z"
  }
  if (is.null(install_dir)) {
    install_dir <- file.path(rappdirs::user_data_dir("openac", "R"), "ffmpeg")
  }
  if (!dir.exists(install_dir)) {
    status <- dir.create(install_dir, recursive = TRUE)
    if (status == FALSE) return(FALSE)
  }
  # Download the installer to a temporary file
  tf <- tempfile()
  status <-
    utils::download.file(
      url = download_url,
      destfile = tf,
      mode = "wb"
    )
  if (status != 0) {
    warning("File download failed")
    return(FALSE)
  }
  # Extract the archive from the temporary file to the install directory
  archive::archive_extract(tf, dir = install_dir, strip_components = 1)
  # Delete the temporary file
  unlink(tf)
  # Update the user config files with the locations of the installed files
  set_ffmpeg(file.path(install_dir, "bin", "ffmpeg.exe"))
  set_ffprobe(file.path(install_dir, "bin", "ffprobe.exe"))
  set_ffplay(file.path(install_dir, "bin", "ffplay.exe"))

  TRUE
}

# install_openface_win() -------------------------------------------------------

#' Install openface on Windows
#'
#' Downloads an openface zip installer, extracts it, and updates the package's
#' user config files to point to the component executable files.
#'
#' @param download_url A string indicating the location of the openface
#'   installation zip file. If `NULL`, will default to the version 2.2.0 x64
#'   installer from github.
#' @param install_dir A string indicating a directory to install openface to. If
#'   `NULL`, will default to installing to the user data directory.
#' @return A logical indicating whether the installation was successful.
#' @export
install_openface_win <- function(download_url = NULL, install_dir = NULL) {

  options(timeout = max(300, getOption("timeout")))

  if (is.null(download_url)) {
    download_url <- paste0(
      "https://github.com/TadasBaltrusaitis/OpenFace/releases/download/",
      "OpenFace_2.2.0/OpenFace_2.2.0_win_x64.zip"
    )
  }
  if (is.null(install_dir)) {
    install_dir <- file.path(rappdirs::user_data_dir("openac", "R"), "openface")
  }
  if (!dir.exists(install_dir)) {
    status <- dir.create(install_dir, recursive = TRUE)
    if (status == FALSE) return(FALSE)
  }
  # Download the installer to a temporary file
  tf <- tempfile()
  status <-
    utils::download.file(
      url = download_url,
      destfile = tf,
      mode = "wb"
    )
  if (status != 0) {
    warning("File download failed")
    return(FALSE)
  }
  # Extract the archive from the temporary file to the install directory
  archive::archive_extract(tf, dir = install_dir, strip_components = 1)
  # Delete the temporary file
  unlink(tf)
  # Update the user config files with the locations of the installed files
  set_openface(file.path(install_dir, "FaceLandmarkVidMulti.exe"))
  # Download patch experts
  status1 <-
    utils::download.file(
      url = "https://onedrive.live.com/download?cid=2E2ADA578BFF6E6E&resid=2E2ADA578BFF6E6E%2153072&authkey=AKqoZtcN0PSIZH4",
      destfile = file.path(install_dir, "model", "patch_experts", "cen_patches_0.25_of.dat"),
      mode = "wb"
    )
  status2 <-
    utils::download.file(
      url = "https://onedrive.live.com/download?cid=2E2ADA578BFF6E6E&resid=2E2ADA578BFF6E6E%2153079&authkey=ANpDR1n3ckL_0gs",
      destfile = file.path(install_dir, "model", "patch_experts", "cen_patches_0.35_of.dat"),
      mode = "wb"
    )
  status3 <-
    utils::download.file(
      url = "https://onedrive.live.com/download?cid=2E2ADA578BFF6E6E&resid=2E2ADA578BFF6E6E%2153074&authkey=AGi-e30AfRc_zvs",
      destfile = file.path(install_dir, "model", "patch_experts", "cen_patches_0.50_of.dat"),
      mode = "wb"
    )
  status4 <-
    utils::download.file(
      url = "https://onedrive.live.com/download?cid=2E2ADA578BFF6E6E&resid=2E2ADA578BFF6E6E%2153070&authkey=AD6KjtYipphwBPc",
      destfile = file.path(install_dir, "model", "patch_experts", "cen_patches_1.00_of.dat"),
      mode = "wb"
    )
  if (any(status1, status2, status3, status4)) {
    warning("File download failed")
    return(FALSE)
  }

  TRUE
}


# install_opensmile_win() ------------------------------------------------------

#' @export
install_opensmile_win <- function(download_url = NULL, install_dir = NULL) {

  if (is.null(download_url)) {
    download_url <- paste0(
      "https://github.com/audeering/opensmile/releases/download/",
      "v3.0.1/opensmile-3.0.1-win-x64.zip"
    )
  }
  if (is.null(install_dir)) {
    install_dir <- file.path(rappdirs::user_data_dir("openac", "R"), "opensmile")
  }
  if (!dir.exists(install_dir)) {
    status <- dir.create(install_dir, recursive = TRUE)
    if (status == FALSE) return(FALSE)
  }
  # Download the installer to a temporary file
  tf <- tempfile()
  status <-
    utils::download.file(
      url = download_url,
      destfile = tf,
      mode = "wb"
    )
  if (status != 0) {
    warning("File download failed")
    return(FALSE)
  }
  # Extract the archive from the temporary file to the install directory
  archive::archive_extract(tf, dir = install_dir, strip_components = 1)
  # Delete the temporary file
  unlink(tf)
  # Update the user config files with the locations of the installed files
  set_opensmile(file.path(install_dir, "bin", "SMILExtract.exe"))

  TRUE
}


