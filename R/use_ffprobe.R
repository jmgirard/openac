# ffprobe ----------------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg (character) The arguments to append to the ffprobe
#'   command line call, in either of two forms. Give a **character vector**
#'   with one CLI token per element and each element is quoted for you at the
#'   process boundary, so a file path may contain spaces or a `$` --- and, on
#'   Windows, a `%TEMP%`-style token, an `&`, a `^` or a `!`. None of those are
#'   expanded, because openac starts the tool directly rather than through a
#'   command interpreter. Give a **single string** and
#'   it is passed through exactly as written, quoting and all, which leaves any
#'   quoting up to you. Prefer the vector form.
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
#' @return A named integer vector with two elements (`Video` and `Audio`)
#' indicating the number of video and audio streams in `infile`. A file that
#' cannot be probed --- one that does not exist, or one ffprobe rejects ---
#' returns `NA` for both counts with a warning naming it, rather than raising an
#' error, so a batch records that file and carries on. A missing ffprobe still
#' errors: it is a problem with the installation, not with the file.
#' @export
#'
ffp_count_streams <- function(infile) {
  # An unprobeable file is that file's outcome, not the end of the batch (GP6).
  # Both failure branches below return NA counts with a warning naming the
  # file, so a `*_dir()` caller can record the row and go on to the next input.
  # The file is named in full rather than by basename: a batch run over
  # subdirectories has many `clip.mp4`s and only one of them failed.
  if (!file.exists(infile)) {
    cli::cli_warn(c(
      "!" = "Cannot count the streams in {.file {infile}}: the file does not exist.",
      "i" = "Returning {.code NA} counts."
    ))
    return(c(Video = NA_integer_, Audio = NA_integer_))
  }

  # Get types for ALL streams. One element per CLI token: run_tool() quotes
  # each one at the process boundary, so `infile` needs no quoting here.
  arg <- c(
    "-v", "error",
    "-show_entries", "stream=codec_type",
    "-of", "csv=p=0",
    infile
  )

  # Run ffprobe. A non-zero exit makes R emit a warning of its own quoting the
  # entire command line -- MEASURED 2026-08-08 (R 4.6.1): `running command
  # ''ls' /nonexistent-zzz 2>&1' had status 1`. It is muffled here and replaced
  # by the message below, which names the file the caller passed rather than the
  # argv it never wrote. Matched on that wording because the condition carries
  # no class to match on.
  stream_types <- withCallingHandlers(
    ffprobe(arg),
    warning = function(w) {
      if (grepl("had status", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # `system2(stdout = TRUE, stderr = TRUE)` reports a non-zero exit in a
  # `status` attribute rather than by erroring (MEASURED, same run), so a
  # rejected file arrives here looking like ordinary output and has to be
  # caught by the attribute.
  status <- attr(stream_types, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    cli::cli_warn(c(
      "!" = "Cannot count the streams in {.file {infile}}: ffprobe exited with
             status {status}.",
      "i" = "Returning {.code NA} counts."
    ))
    return(c(Video = NA_integer_, Audio = NA_integer_))
  }

  # Count occurrences in R
  vcount <- sum(stream_types == "video")
  acount <- sum(stream_types == "audio")
  
  # Construct output vector
  c(Video = vcount, Audio = acount)
}
