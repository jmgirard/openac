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

# install_openface_mac ---------------------------------------------------------

install_openface_mac <- function(install_dir = NULL) {
  if (is.null(install_dir)) {
    install_dir <- file.path(rappdirs::user_data_dir("openac", "R"), "openface")
  }
  if (!dir.exists(install_dir)) {
    status <- dir.create(install_dir, recursive = TRUE)
    if (status == FALSE) return(FALSE)
  }

  sh <- '
    #!/bin/bash
    # script to install openFace for mac 

    brew update
    brew install gcc 
    brew install boost
    brew install tbb
    brew install openblas
    brew install --build-from-source dlib
    brew install wget
    brew install opencv

    git clone https://github.com/TadasBaltrusaitis/OpenFace.git

    mkdir build
    cd build
    cmake -D WITH_OPENMP=ON CMAKE_BUILD_TYPE=RELEASE ..  
    make

    cd ..
    bash download_models.sh 
    cp lib/local/LandmarkDetector/model/patch_experts/*.dat build/bin/model/patch_experts/
  '
}

# install_opensmile_win --------------------------------------------------------

#TODO: write documentation

#' @export
install_opensmile_win <- function(download_url = NULL, install_dir = NULL) {

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

# TODO: Write documentation

#' @export
install_opensmile_mac <- function(
  download_url = NULL, 
  install_dir = NULL,
  arch = c("armv8", "x86_64")
) {
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

