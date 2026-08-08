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
# `shQuote()` is called with no `type`, which is deliberate: its default is
# sh-style on unix and cmd-style on Windows, the same rule this needs, so naming
# a type here would be a second copy of a decision base already makes.
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

run_tool <- function(program, arg) {
  if (!is.character(arg)) {
    cli::cli_abort(
      "{.arg arg} must be a character vector, not {.obj_type_friendly {arg}}."
    )
  }
  if (length(arg) == 0L) {
    cli::cli_abort("{.arg arg} must contain at least one element.")
  }
  if (anyNA(arg)) {
    cli::cli_abort(
      "{.arg arg} must not contain a missing value ({.code NA})."
    )
  }
  args <- if (length(arg) > 1L) shQuote(arg) else arg
  system2(require_program(program), args = args, stdout = TRUE, stderr = TRUE)
}
