# ffprobe ----------------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffprobe command line call.
#' @return A character vector containing the output of ffprobe.
#' @references https://ffmpeg.org/ffprobe.html
#' @aliases ffp
#' @export
#' @examples 
#' ffprobe('-version')
ffprobe <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run ffprobe
  system2(find_ffprobe(), args = arg, stdout = TRUE, stderr = TRUE)
}


# ffp --------------------------------------------------------------------------

#' @rdname ffprobe
#' @export
ffp <- ffprobe


# ffp_count_streams ------------------------------------------------------------

#' Count the streams in a media file
#' 
#' Use ffprobe to count the number of audio and video streams in a media file.
#' 
#' @param infile (string) The filepath to the media file to import.
#' @return A named numeric vector with two elements (`Video` and `Audio`)
#' indicating the number of video and audio streams in `infile`.
#' @export
ffp_count_streams <- function(infile) {
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
  vcount <- length(ffprobe(arg))
  # Check if there is an audio stream
  arg2 <- paste0(
    '-v error',
    ' -select_streams a:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  acount <- length(ffprobe(arg2))
  # Construct output vector
  c(Video = vcount, Audio = acount)
}
