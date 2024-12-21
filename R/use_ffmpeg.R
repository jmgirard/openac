# ffmpeg -----------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffmpeg command line call.
#' @return A character vector containing the output of ffmpeg.
#' @references https://ffmpeg.org/ffmpeg.html
#' @aliases ffm
#' @export
#' @examples 
#' ffmpeg('-version')
#' 
ffmpeg <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run ffmpeg
  system2(find_ffmpeg(), args = arg, stdout = TRUE, stderr = TRUE)
}


# ffm --------------------------------------------------------------------------

#' @rdname ffmpeg
#' @export
#' 
ffm <- ffmpeg
