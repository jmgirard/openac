# ffprobe ----------------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg (character) The arguments to append to the ffprobe
#'   command line call, in either of two forms. Give a **character vector**
#'   with one CLI token per element and each element is quoted for you at the
#'   process boundary, so a file path may contain spaces or a `$` --- and, on
#'   Windows, a `%TEMP%`-style token, an `&`, a `^` or a `!`, all of which were
#'   measured reaching the tool intact because `system2()` puts no command
#'   interpreter in the loop there. Give a **single string** and
#'   it is passed through to the shell exactly as written, quoting and all,
#'   which leaves any quoting up to you. Prefer the vector form.
#' @return A character vector containing the output of ffprobe. Errors if
#'   ffprobe cannot be found.
#' @references https://ffmpeg.org/ffprobe.html
#' @aliases ffp
#' @export
#' @examples 
#' \dontrun{
#' ffprobe('-version')
#' ffprobe(c("-show_entries", "stream=codec_type", "my video.mp4"))
#' }
#' 
ffprobe <- function(arg) {
  run_tool("ffprobe", arg)
}


# ffp --------------------------------------------------------------------------

#' @rdname ffprobe
#' @export
#' 
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
#' 
ffp_count_streams <- function(infile) {
  # Validate inputs
  stopifnot(file.exists(infile))
  
  # Get types for ALL streams. One element per CLI token: run_tool() quotes
  # each one at the process boundary, so `infile` needs no quoting here.
  arg <- c(
    "-v", "error",
    "-show_entries", "stream=codec_type",
    "-of", "csv=p=0",
    infile
  )

  # Run ffprobe
  stream_types <- ffprobe(arg)
  
  # Count occurrences in R
  vcount <- sum(stream_types == "video")
  acount <- sum(stream_types == "audio")
  
  # Construct output vector
  c(Video = vcount, Audio = acount)
}
