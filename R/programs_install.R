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


# Model downloads --------------------------------------------------------------

# The four CEN patch experts OpenFace needs and its release archive does not
# ship, keyed by the filename they must land under.
#
# These were OneDrive links carrying `authkey=` query parameters until
# 2026-08-08, when M16 ran the installer for real and MEASURED all four
# answering **HTTP 200 with a 34 KB login.live.com sign-in page**. Nothing
# reported it: `download.file()` returns 0 for a 200, the file existed and was
# non-empty, and `install_openface_win()` returned TRUE onto four HTML
# documents named `.dat`. How long they had been dead is unknowable, which is
# the point -- see `download_model()` below.
#
# The replacements are OpenFace's own PRIMARY links. Upstream's
# `download_models.ps1` and `download_models.sh` try Dropbox first and fall
# back to OneDrive; openac had copied only the fallback. All four were measured
# alive and serving binary on 2026-08-08.
openface_patch_experts <- c(
  "cen_patches_0.25_of.dat" =
    "https://www.dropbox.com/s/7na5qsjzz8yfoer/cen_patches_0.25_of.dat?dl=1",
  "cen_patches_0.35_of.dat" =
    "https://www.dropbox.com/s/k7bj804cyiu474t/cen_patches_0.35_of.dat?dl=1",
  "cen_patches_0.50_of.dat" =
    "https://www.dropbox.com/s/ixt4vkbmxgab1iu/cen_patches_0.50_of.dat?dl=1",
  "cen_patches_1.00_of.dat" =
    "https://www.dropbox.com/s/2t5t1sdpshzfhpj/cen_patches_1.00_of.dat?dl=1"
)

# The smallest a real patch expert is: the four measured 60.6 MB, 60.6 MB,
# 154.3 MB and 154.3 MB on 2026-08-08. A floor well under the smallest of them
# tolerates an upstream re-release; a sign-in page (~34 KB) is four orders of
# magnitude below it.
#
# A function, not a constant, so the mocked installer tests can lower it rather
# than write 40 MB of fixture to satisfy it. The guard itself is covered by
# tests that feed `download_model()` a page instead of a model.
model_byte_floor <- function() 40e6

# Download one model file and refuse anything that is not one.
#
# A dead link of this shape does not 404. It redirects to a sign-in page and
# answers 200, so `download.file()`'s status says success and the only way to
# tell is to look at what landed. Two independent bars, because either alone
# has a hole: a byte floor passes a large HTML error page, and a content sniff
# passes a truncated download. Returning FALSE with a warning naming the URL --
# rather than aborting -- keeps the installer's documented `logical` contract.
download_model <- function(url, destfile, floor = model_byte_floor()) {
  status <- tryCatch(
    utils::download.file(url = url, destfile = destfile, mode = "wb"),
    error = function(e) {
      warning("Download failed for ", url, ": ", conditionMessage(e))
      -1L
    }
  )
  if (!identical(as.integer(status), 0L)) {
    warning("File download failed for ", url)
    return(FALSE)
  }
  if (!file.exists(destfile)) {
    warning("Download reported success but wrote no file: ", url)
    return(FALSE)
  }
  size <- file.size(destfile)
  if (size < floor) {
    warning(
      "Downloaded ", basename(destfile), " is ", size, " bytes, below the ",
      floor, "-byte floor for a model file -- ", url,
      " is probably serving an error or sign-in page."
    )
    return(FALSE)
  }
  if (starts_with_markup(destfile)) {
    warning(
      basename(destfile), " is a markup document, not a model -- ", url,
      " is serving a sign-in or error page."
    )
    return(FALSE)
  }
  TRUE
}

# Do the first bytes of `path` open an HTML or XML document?
#
# Read as raw: a model file is binary and `readLines()`/`rawToChar()` choke on
# the embedded nul it is certain to contain. The `<!--` needle is not padding:
# the live.com sign-in page measured on 2026-08-08 opens with a copyright
# comment, so a sniff for `<!DOCTYPE` alone would have missed the exact page
# this guard exists for.
starts_with_markup <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  hex <- paste(
    sprintf("%02x", as.integer(readBin(con, "raw", n = 512L))),
    collapse = ""
  )
  any(vapply(
    c("3c21444f43545950", "3c68746d6c", "3c48544d4c", "3c3f786d6c", "3c212d2d"),
    function(needle) grepl(needle, hex, fixed = TRUE),
    logical(1)
  ))
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
  # Download the patch experts, which the release archive does not ship.
  patch_dir <- file.path(install_dir, "model", "patch_experts")
  if (!dir.exists(patch_dir)) {
    if (!dir.create(patch_dir, recursive = TRUE)) return(FALSE)
  }
  for (model in names(openface_patch_experts)) {
    ok <- download_model(
      url = openface_patch_experts[[model]],
      destfile = file.path(patch_dir, model)
    )
    if (!ok) return(FALSE)
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
      # `opensmile-3.0.2-win-x64.zip` was pinned here and MEASURED 404 on
      # 2026-08-08 (M16): the v3.0.2 release has never carried that name. The
      # asset it does carry is below, read off the release's own API listing.
      "v3.0.2/opensmile-3.0.2-windows-x86_64.zip"
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

