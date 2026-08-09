# Platform guards --------------------------------------------------------------

# The `_win`/`_mac` naming convention (DESIGN "Conventions") is the only thing
# that says which platform an installer is for, and nothing used to check it:
# `install_opensmile_win()` on macOS downloaded the Windows archive, extracted
# it, recorded `bin/SMILExtract.exe` as the openSMILE location, and returned
# TRUE. The guards below turn that silent wrong install into an error.

# The platform each name suffix declares, spelled as `Sys.info()[["sysname"]]`.
installer_suffixes <- c(win = "Windows", mac = "Darwin")

# Human names for the platforms and tools that appear in guard messages.
sysname_labels <- c(Windows = "Windows", Darwin = "macOS", Linux = "Linux")
tool_labels <- c(ffmpeg = "FFmpeg", openface = "OpenFace", opensmile = "openSMILE")

# The running platform. Wrapped in a function so tests can mock `Sys.info()`;
# `.Platform$OS.type` is data rather than a call and cannot be mocked.
current_sysname <- function() unname(Sys.info()[["sysname"]])

label_sysname <- function(sysname) {
  if (sysname %in% names(sysname_labels)) sysname_labels[[sysname]] else sysname
}

label_tool <- function(tool) {
  if (tool %in% names(tool_labels)) tool_labels[[tool]] else tool
}

# The exported installers for `tool`, as a named vector mapping each platform to
# the function that installs there. Computed from the namespace's own exports,
# so adding `install_<tool>_<suffix>()` registers it here with no edit.
installers_for <- function(tool) {
  pattern <- paste0(
    "^install_", tool, "_(", paste(names(installer_suffixes), collapse = "|"), ")$"
  )
  found <- grep(pattern, getNamespaceExports("openac"), value = TRUE)
  suffix <- sub(paste0("^install_", tool, "_"), "", found)
  stats::setNames(found, unname(installer_suffixes[suffix]))
}

# Stop unless the running platform is the one this installer targets.
#
# The alternative branch matters as much as the guard itself: a user on Linux
# calling `install_openface_win()` needs to be told openac has no OpenFace
# installer for Linux, not merely that this one is for Windows.
require_os <- function(tool, suffix, call = rlang::caller_env()) {
  target <- installer_suffixes[[suffix]]
  running <- current_sysname()
  if (identical(running, target)) return(invisible(running))

  fn <- paste0("install_", tool, "_", suffix)
  here <- unname(installers_for(tool)[running])
  hint <- if (length(here) && !is.na(here)) {
    c("i" = "Use {.fn {here}} on {label_sysname(running)} instead.")
  } else {
    c(
      "x" = "openac has no automated {label_tool(tool)} installer for
             {label_sysname(running)}.",
      "i" = "Install {label_tool(tool)} yourself, then record its location with
             {.fn {paste0('set_', tool)}}."
    )
  }
  cli::cli_abort(
    c(
      "{.fn {fn}} installs {label_tool(tool)} on {label_sysname(target)}, but
       this machine is running {label_sysname(running)}.",
      hint
    ),
    class = "openac_wrong_os",
    call = call
  )
}


# install_ffmpeg_win -----------------------------------------------------------

#' Install FFmpeg on Windows
#'
#' Downloads an FFmpeg zip installer, extracts it, and updates the package's
#' user config files to point to the component executable files. Note that this
#' function will also install FFprobe.
#'
#' @param download_url A string indicating the location of the FFmpeg
#'   installation zip file. If `NULL`, will default to the latest static
#'   essentials release from gyan.dev.
#' @param install_dir A string indicating a directory to install FFmpeg to. If
#'   `NULL`, will default to installing to the user data directory.
#' @return A logical indicating whether the installation was successful.
#' @export
#' 
install_ffmpeg_win <- function(download_url = NULL, install_dir = NULL) {

  require_os("ffmpeg", "win")
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
  TRUE
}


# install_openface_win ---------------------------------------------------------

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
#' 
install_openface_win <- function(download_url = NULL, install_dir = NULL) {

  require_os("openface", "win")
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
  return(TRUE)
}

# install_opensmile_win --------------------------------------------------------

#' Install openSMILE on Windows
#'
#' Download a prebuilt openSMILE release for Windows and install it into a
#' local directory, then register the `SMILExtract.exe` location with openac.
#'
#' @param download_url An optional string giving the URL of the openSMILE
#'   Windows archive to download. If `NULL`, a pinned official release is used.
#' @param install_dir An optional string giving the directory to install into.
#'   If `NULL`, a per-user data directory (via [rappdirs::user_data_dir()]) is
#'   used.
#' @return A logical: `TRUE` on success, `FALSE` if the download or directory
#'   creation failed.
#' @examples
#' \dontrun{
#' install_opensmile_win()
#' }
#' @export
install_opensmile_win <- function(download_url = NULL, install_dir = NULL) {

  require_os("opensmile", "win")
  if (is.null(download_url)) {
    download_url <- paste0(
      "https://github.com/audeering/opensmile/releases/download/",
      "v3.0.2/opensmile-3.0.2-win-x64.zip"
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
  return(TRUE)
}


# install_opensmile_mac --------------------------------------------------------

#' Install openSMILE on macOS
#'
#' Download a prebuilt openSMILE release for macOS and install it into a local
#' directory, then register the `SMILExtract` location with openac.
#'
#' @param download_url An optional string giving the URL of the openSMILE macOS
#'   archive to download. If `NULL`, a pinned official release matching `arch`
#'   is used.
#' @param install_dir An optional string giving the directory to install into.
#'   If `NULL`, a per-user data directory (via [rappdirs::user_data_dir()]) is
#'   used.
#' @param arch The CPU architecture to install for, either `"armv8"` (Apple
#'   silicon) or `"x86_64"` (Intel). Ignored when `download_url` is supplied.
#' @return A logical: `TRUE` on success, `FALSE` if the download or directory
#'   creation failed.
#' @examples
#' \dontrun{
#' install_opensmile_mac()
#' }
#' @export
install_opensmile_mac <- function(
  download_url = NULL, 
  install_dir = NULL,
  arch = c("armv8", "x86_64")
) {
  require_os("opensmile", "mac")
  # Validate input
  stopifnot(is.null(download_url) || rlang::is_character(download_url, n = 1))
  stopifnot(is.null(install_dir) || rlang::is_character(install_dir, n = 1))
  arch <- match.arg(arch)
  # Prepare download URL
  if (is.null(download_url)) {
    download_url <- paste0(
      "https://github.com/audeering/opensmile/releases/download/v3.0.2/",
      ifelse(
        arch == "armv8",
        "opensmile-3.0.2-macos-armv8.zip",
        "opensmile-3.0.2-macos-x86_64.zip"
      )
    )
  }
  # Prepare install directory
  if (is.null(install_dir)) {
    install_dir <- file.path(rappdirs::user_data_dir("openac", "R"), "opensmile")
  }
  # Create install directory if needed
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
  set_opensmile(
    tools::file_path_as_absolute(
      file.path(install_dir, "bin", "SMILExtract")
    )
  )
  # Return TRUE
  return(TRUE)
}


# install_whisper --------------------------------------------------------------

#' Install audio.whisper package
#' 
#' Install the audio.whisper R package from github.
#' 
#' @return NULL
#' @export
#' 
install_whisper <- function() {
  rlang::check_installed("audio.whisper")
}

