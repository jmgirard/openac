# ffmpeg -----------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg (character) The arguments to append to the ffmpeg
#'   command line call, in either of two forms. Give a **character vector**
#'   with one CLI token per element and each element is quoted for you at the
#'   process boundary, so a file path may contain spaces or a `$`. (One known
#'   gap: on Windows, `%` is not escaped, so a path containing a token such as
#'   `%TEMP%` can still be expanded by the command interpreter.) Give a
#'   **single string** and
#'   it is passed through to the shell exactly as written, quoting and all,
#'   which leaves any quoting up to you. Prefer the vector form.
#' @return A character vector containing the output of ffmpeg. Errors if
#'   ffmpeg cannot be found.
#' @references https://ffmpeg.org/ffmpeg.html
#' @aliases ffm
#' @export
#' @examples 
#' \dontrun{
#' ffmpeg('-version')
#' ffmpeg(c("-i", "my video.mp4", "-c:a", "pcm_s16le", "my audio.wav"))
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
