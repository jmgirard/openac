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

# Run `.f` over the rows of `.l`, surviving a per-file failure (GP6).
#
# A batch that dies on file 412 of 500 overnight is the failure mode to design
# against; a batch that dies silently is worse. Each failure warns, naming the
# file and the condition that stopped it, and the returned table records every
# file's outcome so a caller can re-run exactly the failures.
#
# `parallel = FALSE` is for whisper, whose loop is sequential by design (D-006).
dir_walk <- function(.l, .f, parallel = TRUE) {
  if (nrow(.l) == 0L) {
    return(cbind(.l, success = logical(0), error = character(0)))
  }
  p <- progressr::progressor(steps = nrow(.l))
  step <- function(...) {
    infile <- list(...)$infile
    out <- tryCatch(
      {
        .f(...)
        list(success = TRUE, error = NA_character_)
      },
      error = function(e) {
        cli::cli_warn(
          "Skipping {.file {basename(infile)}}: {conditionMessage(e)}"
        )
        list(success = FALSE, error = conditionMessage(e))
      }
    )
    p(message = basename(infile))
    out
  }
  res <- if (parallel) furrr::future_pmap(.l, step) else purrr::pmap(.l, step)
  cbind(
    .l,
    success = vapply(res, function(x) x$success, logical(1)),
    error = vapply(res, function(x) x$error, character(1)),
    stringsAsFactors = FALSE
  )
}
