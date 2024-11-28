
# ffmpeg ------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   ffmpeg command line call.
#' @return A character vector containing the output of ffmpeg.
#' @references https://ffmpeg.org/ffmpeg.html
#' @export
#' @examples
#' ffmpeg('-version')
ffmpeg <- function(arg) {
  stopifnot(is.character(arg), length(arg) == 1)
  system2(find_ffmpeg(), args = arg, stdout = TRUE, stderr = TRUE)
}

# ffprobe -----------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   ffprobe command line call.
#' @return A character vector containing the output of ffprobe.
#' @references https://ffmpeg.org/ffprobe.html
#' @export
#' @examples
#' ffprobe('-version')
ffprobe <- function(arg) {
  stopifnot(is.character(arg), length(arg) == 1)
  system2(find_ffprobe(), args = arg, stdout = TRUE, stderr = TRUE)
}

count_audio_streams <- function(infile) {
  arg <- paste0(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "',
    infile,
    '"'
  )
  out <- ffprobe(arg)
  length(out)
}

determine_type <- function(infile) {
  arg <- paste0(
    '-v error',
    ' -select_streams v:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  vcheck <- length(ffprobe(arg)) > 0

  arg2 <- paste0(
    '-v error',
    ' -select_streams a:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  acheck <- length(ffprobe(arg2)) > 0

  c(Video = vcheck, Audio = acheck)
}

# check_ffmpeg() ------------------------------------------------------------

#' @export
check_ffmpeg <- function() {

  # Try to find the ffmpeg executable
  ffm <- find_ffmpeg()

  if (is.null(ffm)) {
    return(FALSE)
  }

  # Try to call the openface executable
  res <- try(ffmpeg('-version'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}

# check_ffprobe() -----------------------------------------------------------

#' @export
check_ffprobe <- function() {

  # Try to find the ffmpeg executable
  ffp <- find_ffprobe()

  if (is.null(ffp)) {
    return(FALSE)
  }

  # Try to call the openface executable
  res <- try(ffprobe('-version'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}
