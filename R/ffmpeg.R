
# ffmpeg ------------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  stopifnot(is.character(command), length(command) == 1)
  out <- system(paste0('"', find_ffmpeg(), '" ', command), intern = TRUE)
  out
}
