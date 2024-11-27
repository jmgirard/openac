
# ffmpeg ------------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  stopifnot(is.character(command), length(command) == 1)
  out <- system(paste0('"', find_ffmpeg(), '" ', command), intern = TRUE)
  out
}

# ffprobe -----------------------------------------------------------------

#' @export
ffprobe <- function(command) {
  stopifnot(is.character(command) && length(command) == 1)
  system(paste0('"', find_ffprobe(), '" ', command), intern = TRUE)
}

count_audio_streams <- function(infile) {
  command <- paste0(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "',
    infile,
    '"'
  )
  out <- ffprobe(command)
  length(out)
}

determine_type <- function(infile) {
  command <- paste0(
    '-v error',
    ' -select_streams v:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  vcheck <- length(ffprobe(command)) > 0

  command <- paste0(
    '-v error',
    ' -select_streams a:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  acheck <- length(ffprobe(command)) > 0

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
  res <- try(ffmpeg('-h'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}
