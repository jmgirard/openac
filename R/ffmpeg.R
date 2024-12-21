
# ffmpeg ------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffmpeg command line call.
#' @return A character vector containing the output of ffmpeg.
#' @references https://ffmpeg.org/ffmpeg.html
#' @export
#' @examples 
#' ffmpeg('-version')
ffmpeg <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run ffmpeg
  system2(find_ffmpeg(), args = arg, stdout = TRUE, stderr = TRUE)
}

# ffprobe -----------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffprobe command line call.
#' @return A character vector containing the output of ffprobe.
#' @references https://ffmpeg.org/ffprobe.html
#' @export
#' @examples 
#' ffprobe('-version')
ffprobe <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run ffprobe
  system2(find_ffprobe(), args = arg, stdout = TRUE, stderr = TRUE)
}

# count_audio_streams -------------------------------------------------------

#' Count the audio streams in a media file
#' 
#' Use ffprobe to count the number of audio streams in a media file.
#' 
#' @param infile (string) The filepath to the media file to import.
#' @return An integer indicating the number of audio streams in `infile`.
#' @export
count_audio_streams <- function(infile) {
  # Validate input
  stopifnot(file.exists(infile))
  # Construct ffprobe command
  arg <- paste0(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "',
    infile,
    '"'
  )
  # Run ffprobe command
  out <- ffprobe(arg)
  # Count the number of streams
  length(out)
}

determine_type <- function(infile) {
  # Validate inputs
  stopifnot(file.exists(infile))
  # Check if there is a video stream
  arg <- paste0(
    '-v error',
    ' -select_streams v:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  vcheck <- length(ffprobe(arg)) > 0
  # Check if there is an audio stream
  arg2 <- paste0(
    '-v error',
    ' -select_streams a:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  acheck <- length(ffprobe(arg2)) > 0
  # Construct output vector
  c(Video = vcheck, Audio = acheck)
}

# check_ffmpeg() ------------------------------------------------------------

#' Check that ffmpeg is accessible
#' 
#' Check that ffmpeg is installed and accessible and working properly.
#' 
#' @return A logical indicating whether ffmpeg is working (TRUE) or not (FALSE).
#' @export
#' @examples
#' check_ffmpeg()
check_ffmpeg <- function() {
  # Try to find the ffmpeg executable
  ffm <- find_ffmpeg()
  if (is.null(ffm)) return(FALSE)
  # Try to call the ffmpeg executable
  res <- try(ffmpeg('-version'), silent = TRUE)
  if (inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}

# check_ffprobe() -----------------------------------------------------------

#' Check that ffprobe is accessible
#' 
#' Check that ffprobe is installed and accessible and working properly.
#' 
#' @return A logical indicating whether ffprobe is working (TRUE) or not (FALSE).
#' @export
#' @examples
#' check_ffprobe()
#' @export
check_ffprobe <- function() {
  # Try to find the ffprobe executable
  ffp <- find_ffprobe()
  if (is.null(ffp)) return(FALSE)
  # Try to call the ffprobe executable
  res <- try(ffprobe('-version'), silent = TRUE)
  if(inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}
