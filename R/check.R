# check_ffmpeg -----------------------------------------------------------------

#' Check that ffmpeg is accessible
#' 
#' Check that ffmpeg is installed and accessible and working properly.
#' 
#' @return A logical indicating whether ffmpeg is working (TRUE) or not (FALSE).
#' @export
#' @examples
#' check_ffmpeg()
check_ffmpeg <- function() {
  # Try to find the ffmpeg executable
  ffm <- find_ffmpeg()
  if (is.null(ffm)) return(FALSE)
  # Try to call the ffmpeg executable
  res <- try(ffmpeg('-version'), silent = TRUE)
  if (inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}

# check_ffprobe() --------------------------------------------------------------

#' Check that ffprobe is accessible
#' 
#' Check that ffprobe is installed and accessible and working properly.
#' 
#' @return A logical indicating whether ffprobe is working (TRUE) or not (FALSE).
#' @export
#' @examples
#' check_ffprobe()
check_ffprobe <- function() {
  # Try to find the ffprobe executable
  ffp <- find_ffprobe()
  if (is.null(ffp)) return(FALSE)
  # Try to call the ffprobe executable
  res <- try(ffprobe('-version'), silent = TRUE)
  if(inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}


# check_openface() -------------------------------------------------------------

#' Check that openface is accessible
#' 
#' Check that openface is installed and accessible and working properly.
#' 
#' @return A logical indicating whether openface is working (TRUE) or not 
#'   (FALSE).
#' @export
#' @examples
#' check_openface()
check_openface <- function() {
  # Try to find the openface executable
  of <- find_openface()
  if (is.null(of)) return(FALSE)
  # Try to call the openface executable
  res <- try(openface('-h'), silent = TRUE)
  if(inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}


# check_opensmile() ------------------------------------------------------------

#' Check that opensmile is accessible
#' 
#' Check that opensmile is installed and accessible and working properly.
#' 
#' @return A logical indicating whether opensmile is working (TRUE) or not 
#'   (FALSE).
#' @export
#' @examples
#' check_opensmile()
check_opensmile <- function() {
  # Try to find the opensmile executable
  of <- find_opensmile()
  if (is.null(of)) return(FALSE)
  # Try to call the openface executable
  res <- try(opensmile('-h'), silent = TRUE)
  if(inherits(res, "try-error")) return(FALSE)
  # If not null or error, return TRUE
  return(TRUE)
}