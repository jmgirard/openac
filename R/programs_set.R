# set_program ------------------------------------------------------------------

#' Set the location of a dependency program
#'
#' @param program A string indicating which program to set the location for.
#' @param location A string containing the location of the program.
#' @return Invisibly, `NULL`. Called for its side effect: recording `location`
#'   in the user config directory, where [find_program()] reads it.
#' @export
#' 
set_program <- function(program, location) {
  # Validate arguments
  stopifnot(program %in% c("ffmpeg", "ffprobe", "openface", "opensmile"))
  stopifnot(rlang::is_string(location))
  stopifnot(Sys.which(location) != "")
  # Find where to save user configuration data
  config_dir <- rappdirs::user_config_dir("openac", "R")
  config_file <- file.path(config_dir, paste0(program, "_location.txt"))
  # Create configuration directory if needed
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  # Save location to user configuration file
  writeLines(location, config_file)
}


# set_ffmpeg -------------------------------------------------------------------

#' @rdname set_program
#' @export
#' 
set_ffmpeg <- function(location) {
  set_program("ffmpeg", location)
}


# set_ffprobe ------------------------------------------------------------------

#' @rdname set_program
#' @export
#' 
set_ffprobe <- function(location) {
  set_program("ffprobe", location)
}


# set_openface -----------------------------------------------------------------

#' @rdname set_program
#' @export
#' 
set_openface <- function(location) {
  set_program("openface", location)
}


# set_opensmile ----------------------------------------------------------------

#' @rdname set_program
#' @export
#' 
set_opensmile <- function(location) {
  set_program("opensmile", location)
}

