# ffmpeg -----------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffmpeg command line call.
#' @return A character vector containing the output of ffmpeg. Errors if
#'   ffmpeg cannot be found.
#' @references https://ffmpeg.org/ffmpeg.html
#' @aliases ffm
#' @export
#' @examples 
#' \dontrun{
#' ffmpeg('-version')
#' }
#' 
ffmpeg <- function(arg) {
  run_tool("ffmpeg", arg)
}


# ffm --------------------------------------------------------------------------

#' @rdname ffmpeg
#' @export
#' 
ffm <- ffmpeg
