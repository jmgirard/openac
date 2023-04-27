
# ffmpeg ------------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  stopifnot(is.character(command), length(command) == 1)
  out <- system(paste0('"', find_ffmpeg(), '" ', command), intern = TRUE)
  out
}

# extract_wav() ---------------------------------------------------------

#' @export
extract_wav <- function(infile, outfile, options = "") {

  stopifnot(is.character(infile), length(infile) == 1, file.exists(infile))
  stopifnot(is.character(outfile), length(outfile) == 1)
  stopifnot(is.character(options), length(options) == 1)

  command <- paste0(
    '-i "', infile, '" ',
    options, ' ',
    '-vn -acodec pcm_s16le -ac 2 ',
    '"', outfile, '"'
  )
  ffmpeg(command)
}

# check_ffmpeg() ------------------------------------------------------------

#' @export
check_ffmpeg <- function() {

  # Try to find the opensmile executable
  of <- find_ffmpeg()

  if (is.null(of)) {
    return(FALSE)
  }

  # Try to call the openface executable
  res <- try(ffmpeg('-h'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}
