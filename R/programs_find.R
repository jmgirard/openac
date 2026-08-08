# find_program -----------------------------------------------------------------

#' Find the location of a dependency program
#'
#' Returns the location of the requested program as a string.
#'
#' @param program (character) Which program to find? Can be either "ffmpeg",
#'   "ffprobe", "openface", or "opensmile"
#' @return An absolute path to the program as a string, or `NULL` (with a
#'   warning) if the program could not be found.
#' @export
#'
find_program <- function(program) {
  # Validate arguments
  valid <- c("ffmpeg", "ffprobe", "openface", "opensmile")
  if (!rlang::is_string(program) || !program %in% valid) {
    cli::cli_abort(
      "{.arg program} must be one of {.val {valid}}, not {.val {program}}."
    )
  }
  # First, look for program in path
  location <- Sys.which(program)
  if (location == "") {
    # If program not found, look for a user config file
    config <- file.path(
      rappdirs::user_config_dir("openac", "R"),
      paste0(program, "_location.txt")
    )
    # If no config file exists, the program is simply not found
    if (!file.exists(config)) {
      cli::cli_warn(c(
        "!" = "Failed to find {.pkg {program}}.",
        "i" = "Check that it is installed and, if necessary, use {.fn set_program}."
      ))
      return(NULL)
    }
    # Read the recorded location, ignoring blank lines
    lines <- readLines(config, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    # Resolve what was recorded. set_program() accepts anything Sys.which()
    # resolves, which includes a bare program name on the PATH -- so the
    # RESOLVED path is what gets returned, never the recorded string, which
    # file_path_as_absolute() would reject.
    resolved <- if (length(lines) == 0L) "" else Sys.which(lines[[1]])
    # An empty config file and one naming a vanished binary fail the same way
    if (!nzchar(resolved)) {
      cli::cli_warn(c(
        "!" = "{.pkg {program}} was recorded as being at {.file {config}}, but
               that location no longer resolves to a runnable program.",
        "i" = "Use {.fn set_program} to record its current location."
      ))
      return(NULL)
    }
    location <- resolved
  }
  # Names come from Sys.which(); drop them so the return is a bare string
  unname(tools::file_path_as_absolute(location))
}


# require_program --------------------------------------------------------------

# Resolve a program or stop. `find_program()` warns and returns NULL when a tool
# is absent, and `system2(NULL, args)` runs `args` as a shell command -- so the
# low-level wrappers resolve through here instead, turning a missing tool into
# an error rather than an unintended shell invocation. The warning find_program()
# already emitted carries the set_program() hint, so this only has to stop.
require_program <- function(program) {
  location <- find_program(program)
  if (is.null(location)) {
    cli::cli_abort(
      "Can't run {.pkg {program}} because it could not be found."
    )
  }
  location
}


# find_ffmpeg ------------------------------------------------------------------

#' @rdname find_program
#' @export
#' 
find_ffmpeg <- function() {
  find_program("ffmpeg")
}


# find_ffprobe -----------------------------------------------------------------

#' @rdname find_program
#' @export
#' 
find_ffprobe <- function() {
  find_program("ffprobe")
}


# find_openface ----------------------------------------------------------------

#' @rdname find_program
#' @export
#' 
find_openface <- function() {
  find_program("openface")
}


# find_opensmile ---------------------------------------------------------------

#' @rdname find_program
#' @export
#' 
find_opensmile <- function() {
  find_program("opensmile")
}