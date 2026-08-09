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


# ffp_failed -------------------------------------------------------------------

# Did the ffprobe call behind `out` exit non-zero?
#
# `system2(stdout = TRUE, stderr = TRUE)` reports a non-zero exit in a `status`
# attribute rather than by erroring (MEASURED 2026-08-08, R 4.6.1), so a
# rejected file arrives looking like ordinary output and has to be caught by the
# attribute. A SUCCESSFUL run sets no attribute at all -- NULL, not 0 -- which is
# why `!is.null(status)` comes first.
#
# `!isTRUE(all(status == 0))` rather than a coercion: `as.integer()` on a
# character status warns about the coercion itself, and a status that is NA or
# longer than one element counts as failure without a special case.
# `length(status)` is tested separately because `all()` of an empty vector is
# TRUE, so a zero-length status would otherwise read as a clean exit.
ffp_failed <- function(out) {
  status <- attr(out, "status")
  !is.null(status) && (length(status) == 0L || !isTRUE(all(status == 0)))
}


# ffp_run ----------------------------------------------------------------------

# Run ffprobe, returning its output with the exit status still attached, and
# suppressing R's own report of a non-zero status -- and nothing else.
#
# A non-zero exit makes R emit a warning quoting the entire command line --
# MEASURED 2026-08-08 (R 4.6.1): `running command ''ls' /nonexistent-zzz 2>&1'
# had status 1`. That is the argv the caller never wrote, so callers replace it
# with a message naming the file. This function is where that replacement is
# made possible, and it exists because three call sites need it: the stream
# count below and the second, format-reading query inside `os_check_audio()` and
# `aw_check_audio()`. The two check functions had no such handling until this
# fix, so R's argv dump reached the user from them and the status went unread.
#
# Every warning the call raises is HELD rather than matched, and what happens to
# it is decided afterwards, from the exit status: released on success,
# released-but-for-R's-own on failure, released on error. Keying on the status
# instead of on the message is what makes this work off an English host: R
# translates that warning, and an earlier cut grepped for the literal "had
# status". MEASURED 2026-08-08 (R 4.6.1) -- `LANGUAGE=fr` gives "l'exécution de
# la commande '...' renvoie un statut 1", `LANGUAGE=de` "Ausführung von Kommando
# '...' ergab Status 1". Neither contains the English phrase, so the grep missed
# and a French or German user got both warnings, argv dump included (M14 review
# A1).
#
# Which warning is R's cannot be decided from the text -- that is the locale trap
# A1 was returned for -- so it is decided by POSITION: R warns about the exit
# status after the command has run and returned its output, so its warning is the
# LAST one raised inside the call. Everything before it is released. If that
# premise ever breaks the cost is one stray argv line, not a lost diagnostic.
#
# The error handler is not ceremony. `ffprobe()` aborts when the tool cannot be
# resolved, and `find_program()` WARNS on its way there with the `set_program()`
# hint -- a warning raised inside the held region. Without this, that hint is
# held and then thrown away as the error unwinds past the release below, so a
# user with no ffprobe on their PATH loses the one message telling them how to
# point openac at it. MEASURED while fixing M14's review.
#
# The NESTING is not arbitrary: `tryCatch` goes OUTSIDE, so the handler runs
# after `withCallingHandlers` has unwound. Written the other way round, the
# handler's release loop runs while the calling handler is still established, so
# every warning it re-raises is re-captured and muffled and NOTHING escapes
# (MEASURED both ways at M17's review).
ffp_run <- function(arg) {
  held <- list()
  out <- tryCatch(
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

  if (ffp_failed(out)) {
    # Only R's own status report is ours to suppress -- it is what the callers'
    # messages replace. Anything ELSE raised during the call is a diagnostic the
    # caller should still see, and an earlier cut dropped all of them here, which
    # made a failed probe the one path where a warning could vanish (M14
    # fix-delta review F1).
    for (w in utils::head(held, -1L)) warning(w)
  } else {
    # Nothing here is ours to suppress: re-signal each held condition unchanged,
    # class and all.
    for (w in held) warning(w)
  }

  out
}


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
#' error, so a batch records that file and carries on. Two things still error,
#' because neither is a fact about the file: a missing ffprobe, which is a
#' problem with the installation, and an `infile` that is not a single file
#' path. Note that a warning raised by ffprobe itself reaches you either way ---
#' only R's own report of the exit status is replaced by the message above.
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
  # The same guard the per-file functions run, and the same message: M19's
  # review round 1 found every one of them missing it, so it moved into a shared
  # helper rather than staying written out here alone.
  check_file_arg(infile)

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

  # Run ffprobe. `ffp_run()` holds R's own report of a non-zero exit -- an argv
  # dump the caller never wrote -- and releases every other warning unchanged;
  # the message below is what replaces it. That machinery lives there rather
  # than here because the two `*_check_audio()` functions need it for their
  # second query too.
  stream_types <- ffp_run(arg)

  if (ffp_failed(stream_types)) {
    status <- attr(stream_types, "status")
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
