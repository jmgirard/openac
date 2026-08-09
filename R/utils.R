# Shared internals for the `*_dir()` batch wrappers ----------------------------
#
# Every `*_dir()` function does the same three things: enumerate inputs by
# extension, derive output paths that mirror the input tree, and iterate the
# single-file operation over them. Each used to do all three itself, with
# `gsub()` over path strings, and each had the same defects (M07).

# Escape a string for use as a literal inside a regular expression.
regex_quote <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}

# The files under `indir` whose name ends in `.<inext>`.
#
# The pattern is anchored at both ends of the extension --- `\.mp4$`, not
# `mp4$` --- and `inext` is escaped, so `inext = "mp4"` matches `clip.mp4` and
# `clip.mp4.backup.mp4` but never `clip.notmp4`. `list.files()` matches its
# pattern against file names only, so a *directory* named `mp4` contributes no
# match either. Both were live defects: the old `paste0(inext, "$")` matched any
# name merely ending in those letters.
#
# Matching is case-INsensitive, so `inext = "mp4"` also takes `clip.MP4` ---
# cameras and phones write upper-case extensions routinely, and a batch that
# silently ignored half a directory is worse than one that takes too much. The
# cost is that two inputs differing only in extension case derive one output
# path; `dir_outputs()` refuses that rather than letting one overwrite the other.
dir_inputs <- function(indir, inext, recursive = FALSE) {
  inext <- sub("^\\.", "", inext)
  found <- list.files(
    path = indir,
    pattern = paste0("\\.", regex_quote(inext), "$"),
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  # `list.files(recursive = FALSE)` returns directories alongside files, so a
  # directory named `scenes.mp4` matches the pattern and would be handed to the
  # tool as though it were a clip -- `file.exists()` is TRUE for a directory, so
  # the wrappers' own input check does not catch it either. (`recursive = TRUE`
  # already omits directories unless `include.dirs` asks for them, which is why
  # this only ever bit the flat case.)
  found[!dir.exists(found)]
}

# Output paths under `outdir` mirroring each input's position under `indir`,
# with the extension replaced by `ext`. Creates the directories they need.
#
# Paths are manipulated as paths, by `fs`. The old form was
# `gsub(indir, outdir, infiles)` then `gsub(inext, ext, .)`, which treated
# `indir` as a regular expression --- so an input directory containing `+`,
# `(`, or `.` derived a wrong output path --- and substituted every occurrence
# of the extension anywhere in the path, so `clips/mp4/a.mp4.backup.mp4` became
# `out/wav/a.wav.backup.wav` under a directory that was never meant to move.
#
# Two inputs deriving the SAME output path abort the batch. Because `dir_inputs()`
# matches the extension case-insensitively, `clip.mp4` and `clip.MP4` are both
# inputs on a case-sensitive filesystem and both derive `clip.wav` --- the batch
# would write that one file twice and report success for both, losing one result
# with nothing to show for it. This trades GP6 (skip-and-report over aborting)
# deliberately: the check is pre-flight, so no tool has run and no batch work is
# thrown away, and it matches the sibling `indir` guard above.
dir_outputs <- function(infiles, indir, outdir, ext) {
  rel <- fs::path_rel(fs::path_abs(infiles), start = fs::path_abs(indir))
  if (any(startsWith(as.character(rel), ".."))) {
    cli::cli_abort("All input files must be located under {.arg indir}.")
  }
  out <- fs::path_ext_set(fs::path(fs::path_abs(outdir), rel), ext)
  dupes <- unique(out[duplicated(out)])
  if (length(dupes)) {
    collisions <- vapply(
      dupes,
      function(d) paste(basename(infiles[out == d]), collapse = " and "),
      character(1)
    )
    cli::cli_abort(
      c(
        "Two or more input files would be written to the same output file.",
        "x" = "{.file {basename(dupes)}} would be written from {collisions}.",
        "i" = "Input extensions are matched regardless of case; rename or
               separate the inputs so each derives its own output."
      ),
      class = "openac_output_collision"
    )
  }
  if (length(out)) {
    fs::dir_create(fs::path_dir(out))
  }
  as.character(out)
}

# Abort naming the file the batch stopped on, and the defect (M19).
#
# Every guard inside a per-file function is reachable from `dir_walk()`, whose
# error handler copies `conditionMessage()` straight into the outcome table's
# `error` column. A `stopifnot()` there contributes the DEPARSE of its own
# condition -- `file.exists(infile) is not TRUE` -- which names neither the file
# nor what was wrong with it, and the file name reaches the user only because
# `dir_walk()` prepends a basename to its own warning, which the `error` column
# does not carry. This is that column's message.
#
# The shape matches `run_checked()`'s, deliberately: a batch table whose failed
# rows read two different ways for a missing file and a failed tool is harder to
# scan than one that always leads with the file.
#
# `file` is the file to NAME, which is not always the file the guard tested:
# `os_extract_wav()` is handed a wav derived from the user's input, so a message
# built from what it received names a path the user never chose (M17 review,
# finding B). Callers there pass `source`.
#
# `message` is a cli format string evaluated in a CHILD of the caller's
# environment, so it may interpolate the guarded function's own arguments
# (`{.arg overwrite}`, `{.val {stream}}`) as well as the two bindings added
# here. The file is interpolated as a VALUE, never pasted into the format
# string, so a `{` in a filename cannot be read as glue markup and abort inside
# the abort -- the same rule `run_checked()` follows for tool output.
abort_file <- function(file,
                       message,
                       class = character(),
                       call = rlang::caller_env()) {
  caller <- rlang::caller_env()
  envir <- rlang::env(caller, guarded_name = basename(file), guarded_path = file)
  cli::cli_abort(
    c("Could not process {.file {guarded_name}}.", "x" = message),
    class = c(class, "openac_file_guard"),
    call = call,
    .envir = envir
  )
}

# Run `run` under the progress mode the caller asked for, returning its value.
with_progress_mode <- function(run, progress = c("auto", "on", "off")) {
  progress <- match.arg(progress)
  if (progress == "on") {
    progressr::with_progress(run())
  } else if (progress == "off") {
    progressr::without_progress(run())
  } else {
    run()
  }
}

# Signal that the caller deliberately did not process this file (M18).
#
# NOT an error. A condition signalled with no handler established simply
# returns, so a DIRECT call to a single-file function is unchanged --
# `aw_prep_audio(overwrite = FALSE)` still returns "Skipped" to a user who
# calls it by hand. Under `dir_walk()` a handler IS established, and it unwinds
# the call and records the row as skipped. That asymmetry is the point: the
# batch learns the file was declined without the exported functions gaining an
# error branch none of their callers asked for.
#
# `reason` is plain prose, not a cli template -- it is interpolated into
# `dir_walk()`'s message as a value, so braces in it would be confusing at best.
skip_file <- function(reason) {
  rlang::signal(reason, class = "openac_file_skipped")
}

# Run `expr`, absorbing a `skip_file()` raised inside it (M18 review round 1).
#
# `overwrite = FALSE` means "reuse the audio you already prepared". Where the
# preparing is the WHOLE job -- `os_prep_audio_dir()`, `aw_prep_audio_dir()` --
# there is then nothing to do and the batch records a skip. Where it is one
# stage of a larger job -- `os_extract()` going on to run openSMILE,
# `aw_transcribe()` going on to run whisper -- the reuse is the fast path, and
# the skip must stop at the prep call.
#
# It must stop HERE rather than be sorted out in `dir_walk()`, because that
# handler is EXITING: a signal reaching it unwinds the whole per-file job. It
# did, and MEASURED before this fix, `os_extract_dir(wavdir=, aggdir=,
# overwrite = FALSE)` over a file whose wav already existed never called
# openSMILE, wrote no CSV, and recorded `status = "skipped"` -- a deliberate
# skip of work the caller did want done.
#
# Returns what the prep functions return when they decline, so a direct call to
# the wrapping function sees exactly what it saw before the skip channel
# existed.
absorb_skip <- function(expr) {
  tryCatch(expr, openac_file_skipped = function(cnd) "Skipped")
}

# Run `.f` over the rows of `.l`, surviving a per-file failure (GP6).
#
# A batch that dies on file 412 of 500 overnight is the failure mode to design
# against; a batch that dies silently is worse. Each failure warns, naming the
# file and the condition that stopped it, and the returned table records every
# file's outcome so a caller can re-run exactly the failures.
#
# THREE outcomes, not two (M18). `status` is the authority --- "ok" for a file
# the operation completed, "skipped" for one it deliberately declined
# (`skip_file()` above), "failed" for one that errored --- and `success` is
# `status == "ok"`, kept because it is the column callers already read. A skip
# and a failure both read `success = FALSE`, so `success` alone cannot tell "I
# chose not to" from "I tried and could not"; before M18 a skip read
# `success = TRUE` and the batch reported work it had not done.
#
# A failure WARNS and a skip only informs. Re-running a finished batch with
# `overwrite = FALSE` skips every file, and 500 warnings for a batch behaving
# exactly as asked would bury the rows a caller must actually act on.
#
# `parallel = FALSE` is for whisper, whose loop is sequential by design (D-006).
dir_walk <- function(.l, .f, parallel = TRUE) {
  if (nrow(.l) == 0L) {
    return(cbind(
      .l,
      status = character(0),
      success = logical(0),
      error = character(0)
    ))
  }
  p <- progressr::progressor(steps = nrow(.l))
  step <- function(...) {
    infile <- list(...)$infile
    out <- tryCatch(
      {
        .f(...)
        list(status = "ok", success = TRUE, error = NA_character_)
      },
      openac_file_skipped = function(cnd) {
        cli::cli_alert_info(
          "Skipping {.file {basename(infile)}}: {conditionMessage(cnd)}"
        )
        list(
          status = "skipped",
          success = FALSE,
          error = conditionMessage(cnd)
        )
      },
      error = function(e) {
        cli::cli_warn(
          "Skipping {.file {basename(infile)}}: {conditionMessage(e)}"
        )
        list(status = "failed", success = FALSE, error = conditionMessage(e))
      }
    )
    p(message = basename(infile))
    out
  }
  res <- if (parallel) furrr::future_pmap(.l, step) else purrr::pmap(.l, step)
  cbind(
    .l,
    status = vapply(res, function(x) x$status, character(1)),
    success = vapply(res, function(x) x$success, logical(1)),
    error = vapply(res, function(x) x$error, character(1)),
    stringsAsFactors = FALSE
  )
}
