# run_tool ---------------------------------------------------------------------

# The one place openac quotes for the shell (M13, D-017).
#
# `system2()` does NOT quote its `args`: it pastes them into a command string
# that the shell then re-splits (MEASURED -- `args = c("-i", "a b.mp4")` reaches
# the tool as three arguments, `-i`, `a`, `b.mp4`). Something must therefore
# quote, and until M13 every caller did it by hand, interpolating literal `"`
# around each path at its own call site. That is a bug per call site rather than
# one bug: `paste0('-i "', path, '"')` survives a space and loses to a `$`,
# because the shell expands inside double quotes -- `/tmp/a $b.mp4` was measured
# reaching the tool as `/tmp/a .mp4`.
#
# `arg` has two forms and its LENGTH decides which (D-017):
#
#   length 1   the legacy raw string, passed through untouched. It is already a
#              whole command line, quoting and all, and the caller owns it.
#              Quoting it would collapse the line into one argument.
#   length > 1 one CLI token per element, `shQuote()`d individually. This is the
#              form every openac assembler emits.
#
# The quoting STYLE is `sh` on unix and `cmd` on Windows -- base `shQuote()`'s
# own platform default, now named explicitly (`quote_type()`) so the Windows
# rule can be asserted from a macOS or Linux host rather than only from Windows.
# Naming it changes no behavior: MEASURED 2026-08-08 on Windows 11 (build 26100,
# R 4.6.1), `shQuote(x)` and `shQuote(x, type = "cmd")` are identical for every
# entry of the hostile-name table.
#
# `cmd` style wraps in double quotes and escapes nothing else -- it leaves `%`,
# `^`, `&` and `!` bare, all of which `cmd.exe` acts on -- and M13 recorded that
# as an open Windows hole by analogy with the `$` bug it had just fixed. M15
# MEASURED the analogy false: on that host all eight hostile names round-tripped
# through real ffmpeg and ffprobe intact, `a %TEMP% token.wav` included. That one
# entry is what carries the conclusion -- `cmd.exe` leaves `^`, `&` and a
# backtick alone inside double quotes and expands `!` only under delayed
# expansion, so those four would have survived an interpreter too, while `%VAR%`
# is the one thing `cmd.exe` DOES expand inside double quotes. It arrived
# unexpanded, so nothing interpreted it: `system2()` put no `cmd.exe` between
# openac and the tool, and the `cmd2` escaping style (`^%`, `^&`, `^!`), which
# exists for command lines that DO reach the interpreter, would be escaping
# against a shell that is not there. Hence `cmd` alone, on measurement rather
# than on `shQuote`'s documented default.
#
# What that measurement does NOT cover, so the next maintainer knows where its
# edge is: it is one Windows build, one R version, two tools, and openac's own
# `stdout = TRUE, stderr = TRUE` call shape -- and `?system2` ties the no-shell
# property to redirection handling specifically. Re-measure with the same
# hostile-name table before widening the claim to a different call shape.
#
# Resolution stays in `require_program()` rather than moving here, because that
# guard is what stops `system2(NULL, args)` from executing `args` as a shell
# command when a tool is absent (M06).
# An optional run of tokens, or none (M13).
#
# The token form has no equivalent of the empty string that `ifelse(flag, " -x",
# "")` relied on: `""` is a real, empty argument once quoted, and the tool sees
# it. `character()` is the right absence -- it disappears inside the enclosing
# `c()` -- so optional flags go through here rather than through `ifelse()`.
opt_arg <- function(test, ...) {
  if (isTRUE(test)) c(...) else character()
}

# The quoting rule as a value, so a test can ask for the Windows one from any
# host (M15, AC4). `run_tool()` is the only caller in package code; a test that
# wants a specific style names it to `quote_tokens()` directly rather than going
# through here, which is the whole point of the split.
quote_type <- function() {
  if (.Platform$OS.type == "windows") "cmd" else "sh"
}

# The length rule of D-017, applied under a named style. Kept separate from
# `run_tool()` because that function cannot be called without a resolvable
# program and a `system2()` boundary to catch, and the quoting is what needs
# asserting character by character.
quote_tokens <- function(arg, type) {
  if (length(arg) > 1L) shQuote(arg, type = type) else arg
}

run_tool <- function(program, arg) {
  if (!is.character(arg)) {
    cli::cli_abort(
      "{.arg arg} must be a character vector, not {.obj_type_friendly {arg}}.",
      call = rlang::caller_env()
    )
  }
  if (length(arg) == 0L) {
    cli::cli_abort(
      "{.arg arg} must contain at least one element.",
      call = rlang::caller_env()
    )
  }
  if (anyNA(arg)) {
    cli::cli_abort(
      "{.arg arg} must not contain a missing value ({.code NA}).",
      call = rlang::caller_env()
    )
  }
  args <- quote_tokens(arg, type = quote_type())
  system2(require_program(program), args = args, stdout = TRUE, stderr = TRUE)
}


# run_checked ------------------------------------------------------------------

# Run `program` on behalf of `infile`, aborting if the tool exits non-zero.
#
# `run_tool()` returns `system2()`'s value verbatim, and `system2(stdout = TRUE,
# stderr = TRUE)` reports a non-zero exit in a `status` attribute rather than by
# erroring. So before M17 a failed ffmpeg, openSMILE or OpenFace run returned
# normally from every per-file wrapper, and `dir_walk()` -- which classifies a
# row only by whether the call raised an error (R/utils.R) -- recorded the file
# as a SUCCESS with no output written. `ffp_count_streams()` was the only place
# in the package that read a status at all.
#
# The check lives here, called by the wrappers that know which file is being
# processed, rather than inside `run_tool()`, which does not: the message has to
# name the file, and `ffp_count_streams()` reads the status itself to return its
# contractual `NA` counts (M14) and would need an opt-out. The four exported
# passthroughs keep returning the output unchanged either way -- they are the
# documented low-level escape hatch.
#
# MEASURED 2026-08-08 (R 4.6.1, macOS 15, ffmpeg 8.0): a SUCCESSFUL run sets no
# `status` attribute at all -- NULL, not 0. `!is.null(status)` first is what
# keeps this from aborting every successful call. `length(status) == 0L` is
# tested separately because `all()` of an empty vector is TRUE, so a zero-length
# status would otherwise read as a clean exit; `system2()` sets no such
# attribute today, and this costs one comparison and removes the question.
run_checked <- function(program, arg, infile, call = rlang::caller_env()) {
  # R's own exit-status warning is TRANSLATED (LESSONS, M14), so which warning
  # is ours to suppress cannot be decided from its text. It is decided by
  # POSITION: R warns about the status after the command has run and returned,
  # so its warning is the LAST one raised inside the call. Everything before it
  # is a diagnostic the caller should still see, and is released unchanged.
  held <- list()
  out <- withCallingHandlers(
    tryCatch(
      run_tool(program, arg),
      error = function(e) {
        for (w in held) warning(w)
        stop(e)
      }
    ),
    warning = function(w) {
      held[[length(held) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  status <- attr(out, "status")
  failed <- !is.null(status) &&
    (length(status) == 0L || !isTRUE(all(status == 0)))
  if (!failed) {
    for (w in held) warning(w)
    return(out)
  }

  for (w in utils::head(held, -1L)) warning(w)

  # What the tool itself said, which is the difference between a report and a
  # shrug: the batch table's `error` column is where a user reads this, and
  # without it they must re-run the file by hand to learn why it failed. Capped
  # at the last few lines so one bad file cannot flood a data frame column --
  # ffmpeg is verbose, and the operative complaint is always at the end.
  #
  # Interpolated as a VALUE, never pasted into the format string: the tool's
  # output is untrusted text, and a `{` in it would be read as glue markup and
  # abort inside the handler -- a failure while reporting a failure, which is
  # how the batch loses the row it was trying to record.
  said <- utils::tail(as.character(out), 3L)
  said <- said[nzchar(said)]
  reported <- if (length(status) == 0L) "no status" else paste(status, collapse = ", ")

  cli::cli_abort(
    c(
      "Could not process {.file {basename(infile)}}.",
      "x" = "{program} exited with status {reported}.",
      if (length(said)) c("i" = "{program} said: {said}")
    ),
    class = "openac_tool_failed",
    call = call
  )
}
