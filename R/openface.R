
# openface ---------------------------------------------------------------------

#' @export
openface <- function(command) {
  stopifnot(is.character(command), length(command) == 1)
  out <- system(paste0('"', find_openface(), '" ', command), intern = TRUE)
  out
}
