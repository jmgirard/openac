
# openface() -------------------------------------------------------------------

#' Low-level access to the openface command line interface
#'
#' Attempt to find and run openface with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   FaceLandmarkVidMulti.exe command line call.
#' @return A character vector containing the output of openface.
#' @references https://github.com/TadasBaltrusaitis/OpenFace/wiki/Command-line-arguments
#' @export
#' @examples
#' openface('-h')
openface <- function(arg) {
  stopifnot(is.character(arg), length(arg) == 1)
  system2(find_openface(), args = arg, stdout = TRUE, stderr = TRUE)
}


# extract_openface() -----------------------------------------------------------

#' @export
extract_openface <- function(infile, outfile,
                             fp2D = TRUE, fp3D = TRUE, pdm = FALSE,
                             pose = TRUE, gaze = TRUE, aus = TRUE,
                             wild = FALSE, multiview = FALSE) {

  stopifnot(
    is.character(infile), length(infile) == 1,
    is.character(outfile), length(outfile) == 1,
    is.logical(fp2D), is.logical(fp3D), is.logical(pdm),
    is.logical(pose), is.logical(gaze), is.logical(aus),
    is.logical(wild), is.logical(multiview)
  )

  arg <- paste0(
    '-f "', infile, '" ',
    '-of "', outfile, '" ',
    ifelse(fp2D, '-2Dfp ', ''),
    ifelse(fp3D, '-3Dfp ', ''),
    ifelse(pdm, '-pdmparams ', ''),
    ifelse(pose, '-pose ', ''),
    ifelse(gaze, '-gaze ', ''),
    ifelse(aus, '-aus ', ''),
    ifelse(wild, '-wild ', ''),
    ifelse(multiview, '-multi_view 1', '')
  )

  openface(arg)
}


# check_openface() -------------------------------------------------------------

#' @export
check_openface <- function() {

  # Try to find the openface executable
  of <- find_openface()

  if (is.null(of)) {
    return(FALSE)
  }

  # Try to call the openface executable
  res <- try(openface('-h'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}
