# openface ---------------------------------------------------------------------

#' Low-level access to the openface command line interface
#'
#' Attempt to find and run openface with the specified arguments.
#'
#' @param arg (string) Space-separated arguments to append to the
#'   FaceLandmarkVidMulti.exe command line call.
#' @return A character vector containing the output of openface.
#' @references https://github.com/TadasBaltrusaitis/OpenFace/wiki/Command-line-arguments
#' @aliases of
#' @export
#' @examples
#' openface('-h')
#' 
openface <- function(arg) {
  # Validate input
  stopifnot(rlang::is_string(arg))
  # Run openface
  system2(find_openface(), args = arg, stdout = TRUE, stderr = TRUE)
}


# of ---------------------------------------------------------------------------

#' @rdname openface
#' @export
#' 
of <- openface


# of_extract -------------------------------------------------------------------

#' Extract openface features
#' 
#' Extract openface features from a video with potentially multiple faces using
#' FaceLandmarkVidMulti.exe and the specified arguments.
#' 
#' @param infile (string) What is the filepath of the video file?
#' @param outfile (string) What filepath (.csv) should the output be written to? 
#' @param fp2D (logical, default=TRUE) Should the output include 2D facial landmark points (in pixels)?
#' @param fp3D (logical, default=TRUE) Should the output include 3D facial landmark points (in millimeters)?
#' @param pdm (logical, default=FALSE) Should the output include the parameter estimates of the point distribution model?
#' @param pose (logical, default=TRUE) Should the output include head pose estimates?
#' @param gaze (logical, default=TRUE) Should the output include eye gaze estimates?
#' @param aus (logical, default=TRUE) Should the output include action unit estimates?
#' @param wild (logical, default=FALSE) Should the model consider extended search regions (for challenging images)?
#' @param multiview (logical, default=FALSE) Should multi-view initialisation be used (more robust but slower)?
#' @return A character vector containing openface output.
#' @references https://github.com/TadasBaltrusaitis/OpenFace/wiki/Command-line-arguments
#' @export
#' 
of_extract <- function(
  infile, 
  outfile,
  fp2D = TRUE, 
  fp3D = TRUE, 
  pdm = FALSE,
  pose = TRUE, 
  gaze = TRUE, 
  aus = TRUE,
  wild = FALSE, 
  multiview = FALSE
) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_string(outfile))
  stopifnot(rlang::is_bool(fp2D))
  stopifnot(rlang::is_bool(fp3D))
  stopifnot(rlang::is_bool(pdm))
  stopifnot(rlang::is_bool(pose))
  stopifnot(rlang::is_bool(gaze))
  stopifnot(rlang::is_bool(aus))
  stopifnot(rlang::is_bool(wild))
  stopifnot(rlang::is_bool(multiview))
  # Construct openface command
  arg <- paste0(
    '-f "', infile, '"',
    ' -of "', outfile, '"',
    ifelse(fp2D, ' -2Dfp', ''),
    ifelse(fp3D, ' -3Dfp', ''),
    ifelse(pdm, ' -pdmparams', ''),
    ifelse(pose, ' -pose', ''),
    ifelse(gaze, ' -gaze', ''),
    ifelse(aus, ' -aus', ''),
    ifelse(wild, ' -wild', ''),
    ifelse(multiview, ' -multi_view 1', '')
  )
  # Run openface command
  openface(arg)
}


# of_extract_dir ---------------------------------------------------------------

#' Run of_extract() on multiple files in a directory
#' 
#' Find all video files with a specified extension in a specified directory and
#' then extract openface features from each. 
#' 
#' Can be optionally run in parallel by running \code{\link[future]{plan}()} 
#' beforehand, e.g., by calling `plan("multisession", workers = 4)`.
#' 
#' Can optionally output a progress bar by using 
#' \code{\link[progressr]{handlers}()} beforehand, e.g., by calling 
#' `handlers("cli"); handlers(global = TRUE)`.
#' 
#' @param indir (character) What directory are the input files in?
#' @param inext (character) What file extension should be looked for in `indir` 
#'   (e.g., "mp4" or "avi")?
#' @param outdir (character) What directory should the output files be created
#'   in?
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @inheritDotParams of_extract fp2D fp3D pdm pose gaze aus wild multiview
#' @return `NULL`
#' @export
#' 
of_extract_dir <- function(
  indir, 
  inext, 
  outdir, 
  recursive = FALSE,
  ...
) {
  # Validate input
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(rlang::is_string(outdir))
  stopifnot(rlang::is_bool(recursive))
  # Find input filepaths
  infiles <- list.files(
    path = indir,
    pattern = paste0(inext, "$"),
    full.names = TRUE,
    recursive = recursive
  )
  # Build output filepaths
  outfiles <- gsub(indir, outdir, infiles)
  outfiles <- gsub(inext, "csv", outfiles)
  # Iterate of_extract() over infiles
  p <- progressr::progressor(along = infiles)
  furrr::future_pwalk(
    .l = data.frame(
      infile = infiles,
      outfile = outfiles
    ),
    .f = function(infile, outfile) {
      of_extract(infile, outfile, ...)
      p() # update progress
    }
  )
}


# of_read ----------------------------------------------------------------------

#' Read OpenFace output into a tidy tibble
#'
#' Read an OpenFace output CSV (as written by [of_extract()]) into a wide
#' [tibble][tibble::tibble] with one row per detected face per frame (OpenFace
#' uses a multi-face model, so a frame with several faces yields several rows
#' sharing a `frame` but differing in `face_id`). Metadata columns (`frame`,
#' `face_id`, `timestamp`, `confidence`, `success`) come first, followed by
#' whichever feature blocks OpenFace emitted (gaze, head pose, 2D/3D facial
#' landmarks, PDM parameters, and action-unit intensities `AU*_r` and
#' presences `AU*_c`), all passed through as-is.
#'
#' OpenFace writes space-padded column headers (e.g. `" confidence"`); the
#' leading/trailing whitespace is stripped from the column names.
#'
#' @param file (character) Path to an OpenFace output CSV.
#' @return A [tibble][tibble::tibble] with one row per detected face per frame
#' and one column per OpenFace metadata field and feature.
#' @seealso [of_extract()], which produces the output file.
#' @examples
#' \dontrun{
#' of_extract("video.mp4", outfile = "video.csv")
#' faces <- of_read("video.csv")
#' }
#' @export
of_read <- function(file) {
  # Validate input
  if (!rlang::is_string(file)) {
    cli::cli_abort(
      "{.arg file} must be a single string, not {.obj_type_friendly {file}}."
    )
  }
  if (!file.exists(file)) {
    cli::cli_abort("Can't find the file {.file {file}}.")
  }
  header <- readLines(file, n = 1L, warn = FALSE)
  if (length(header) == 0L || !nzchar(header)) {
    cli::cli_abort("The file {.file {file}} is empty.")
  }
  df <- read.csv(
    file = file,
    check.names = FALSE,    # preserve OpenFace column names verbatim...
    stringsAsFactors = FALSE
  )
  names(df) <- trimws(names(df))  # ...then strip OpenFace's header padding
  tibble::as_tibble(df)
}
