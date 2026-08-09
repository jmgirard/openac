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
  # A malformed `infile` is a programming error, not a bad file, so it aborts
  # rather than joining the NA outcomes below. The check is not decoration: the
  # `if (!file.exists(infile))` guard that follows takes a length-1 condition,
  # so without it a length-2 `infile` dies on base R's "the condition has
  # length > 1" and `character(0)` on "argument is of length zero" -- both raw,
  # both from an exported function whose contract is to warn rather than stop.
  # (The `stopifnot()` this replaced accepted a vector and passed vacuously on
  # `character(0)`; neither shape was ever meaningful here.)
  if (!rlang::is_string(infile)) {
    cli::cli_abort(
      "{.arg infile} must be a single file path, not
       {.obj_type_friendly {infile}}."
    )
  }

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
  # ''ls' /nonexistent-zzz 2>&1' had status 1`. That is the argv the caller
  # never wrote, so it is replaced below by a message naming the file.
  #
  # Every warning the call raises is held rather than matched, and released
  # again only if the probe turns out to have SUCCEEDED. Keying on the exit
  # status instead of on the message is what makes this work off an English
  # host: R translates that warning, and an earlier cut of this code grepped
  # for the literal "had status". MEASURED 2026-08-08 (R 4.6.1) --
  # `LANGUAGE=fr` gives "l'exécution de la commande '...' renvoie un statut 1",
  # `LANGUAGE=de` "Ausführung von Kommando '...' ergab Status 1". Neither
  # contains the English phrase, so the grep missed and a French or German user
  # got both warnings, including the argv dump (M14 review A1). Holding the
  # conditions themselves, rather than their text, also keeps a warning that is
  # NOT about the exit status -- and would otherwise be swallowed -- reaching
  # the caller intact on the success path.
  # The error handler is not ceremony. `ffprobe()` aborts when the tool cannot
  # be resolved, and `find_program()` WARNS on its way there with the
  # `set_program()` hint -- a warning raised inside the held region. Without
  # this, that hint is held and then thrown away as the error unwinds past the
  # release below, so a user with no ffprobe on their PATH lost the one message
  # telling them how to point openac at it. MEASURED while fixing M14's review.
  held <- list()
  stream_types <- tryCatch(
    withCallingHandlers(
      ffprobe(arg),
      warning = function(w) {
        held[[length(held) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      for (w in held) warning(w)
      stop(e)
    }
  )

  # `system2(stdout = TRUE, stderr = TRUE)` reports a non-zero exit in a
  # `status` attribute rather than by erroring (MEASURED, same run), so a
  # rejected file arrives here looking like ordinary output and has to be
  # caught by the attribute. `!isTRUE(all(status == 0))` rather than a coercion:
  # `as.integer()` on a character status warns about the coercion itself, from
  # inside a function whose contract is one warning, and a status that is NA or
  # longer than one element counts as failure without a special case.
  status <- attr(stream_types, "status")
  if (!is.null(status) && !isTRUE(all(status == 0))) {
    cli::cli_warn(c(
      "!" = "Cannot count the streams in {.file {infile}}: ffprobe exited with
             status {status}.",
      "i" = "Returning {.code NA} counts."
    ))
    return(c(Video = NA_integer_, Audio = NA_integer_))
  }

  # The probe succeeded, so nothing here is ours to suppress: re-signal each
  # held condition unchanged, class and all.
  for (w in held) warning(w)

  # Count occurrences in R
  vcount <- sum(stream_types == "video")
  acount <- sum(stream_types == "audio")
  
  # Construct output vector
  c(Video = vcount, Audio = acount)
}
