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
