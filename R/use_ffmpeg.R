# ffmpeg -----------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg (character) The arguments to append to the ffmpeg
#'   command line call, in either of two forms. Give a **character vector**
#'   with one CLI token per element and each element is quoted for you at the
#'   process boundary, so a file path may contain spaces or a `$` --- and, on
#'   Windows, a `%TEMP%`-style token, an `&`, a `^` or a `!`, all of which were
#'   measured reaching the tool intact because `system2()` puts no command
#'   interpreter in the loop there. Give a **single string** and
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
