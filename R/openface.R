
# openface() -------------------------------------------------------------------

#' Low-level access to the openface command line interface
#'
#' Attempt to find and run openface with the specified arguments.
#'
#' @param arg (string) Space-separated arguments to append to the
#'   FaceLandmarkVidMulti.exe command line call.
#' @return A character vector containing the output of openface.
#' @references https://github.com/TadasBaltrusaitis/OpenFace/wiki/Command-line-arguments
#' @export
#' @examples
#' openface('-h')
openface <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run openface
  system2(find_openface(), args = arg, stdout = TRUE, stderr = TRUE)
}


# extract_openface() -----------------------------------------------------------

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
extract_openface <- function(infile, outfile,
                             fp2D = TRUE, fp3D = TRUE, pdm = FALSE,
                             pose = TRUE, gaze = TRUE, aus = TRUE,
                             wild = FALSE, multiview = FALSE) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_character(outfile, n = 1))
  stopifnot(rlang::is_logical(fp2D, n = 1))
  stopifnot(rlang::is_logical(fp3D, n = 1))
  stopifnot(rlang::is_logical(pdm, n = 1))
  stopifnot(rlang::is_logical(pose, n = 1))
  stopifnot(rlang::is_logical(gaze, n = 1))
  stopifnot(rlang::is_logical(aus, n = 1))
  stopifnot(rlang::is_logical(wild, n = 1))
  stopifnot(rlang::is_logical(multiview, n = 1))
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


# check_openface() -------------------------------------------------------------

#' Check that openface is accessible
#' 
#' Check that openface is installed and accessible and working properly.
#' 
#' @return A logical indicating whether openface is working (TRUE) or not (FALSE).
#' @export
#' @examples
#' check_openface()
#' @export
check_openface <- function() {
  # Try to find the openface executable
  of <- find_openface()
  if (is.null(of)) return(FALSE)
  # Try to call the openface executable
  res <- try(openface('-h'), silent = TRUE)
  if(inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}
