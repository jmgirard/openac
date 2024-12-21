# find_program -----------------------------------------------------------------

#' Find the location of a dependency program
#'
#' Returns the location of the requested program as a string.
#'
#' @param program (character) Which program to find? Can be either "ffmpeg", 
#'   "ffprobe", "openface", or "opensmile"
#' @return Either a string indicating whether the requested program was found or
#'   `NULL` if the program could not be found.
#' @export
#' 
find_program <- function(program) {
  # Validate arguments
  stopifnot(program %in% c("ffmpeg", "ffprobe", "openface", "opensmile"))
  # First, look for program in path
  location <- Sys.which(program)
  if (location == "") {
    # If program not found, look for a user config file
    config <- file.path(
      rappdirs::user_config_dir("openac", "R"),
      paste0(program, "_location.txt")
    )
    # If a user config file exists, read it in
    if (file.exists(config)) {
      location <- readLines(config)
      # Verify that the location in the user config file is valid
      if (Sys.which(location) == "") {
        warning(
          paste0(
            program,
            " was set as being at ",
            location,
            " but this file does not seem to exist anymore."
          )
        )
        location <- NULL
      }
    } else {
      # If config file not found, return NULL value and warning
      location <- NULL
      warning(
        paste0(
          "Failed to find ",
          program,
          ". Check that it is installed and, if necessary, ",
          "use the set_program() function."
        )
      )
    }
  }
  tools::file_path_as_absolute(location)
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