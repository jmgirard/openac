
# openface() -------------------------------------------------------------------

#' @export
openface <- function(command) {
  stopifnot(is.character(command), length(command) == 1)
  out <- system(paste0('"', find_openface(), '" ', command), intern = TRUE)
  out
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

  command <- paste0(
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

  openface(command)
}
