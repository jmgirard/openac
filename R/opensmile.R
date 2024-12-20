
# opensmile() ------------------------------------------------------------------

#' Low-level access to the opensmile command line interface
#'
#' Attempt to find and run opensmile with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   SMILEextract command line call.
#' @return A character vector containing the output of openface.
#' @references https://audeering.github.io/opensmile/
#' @export
#' @examples
#' opensmile('-h')
opensmile <- function(arg) {
  stopifnot(is.character(arg), length(arg) == 1)
  system2(find_opensmile(), args = arg, stdout = TRUE, stderr = TRUE)
}


# opensmile_configs() ----------------------------------------------------------

#' @export
opensmile_configs <- function() {
  fd <- dirname(find_opensmile())
  configs <- list.files(
    path = file.path(fd, "..", "config"),
    pattern = ".conf$",
    full.names = FALSE,
    recursive = TRUE
  )
  configs
}


# check_config() ---------------------------------------------------------------

#' @export
check_config <- function(config) {
  config_sans <- tools::file_path_sans_ext(config)
  configs_sans <- tools::file_path_sans_ext(opensmile_configs())
  if (config_sans %in% configs_sans) {
    fd <- dirname(find_opensmile())
    config <- file.path(fd, "..", "config", paste0(config_sans, ".conf"))
    config <- tools::file_path_as_absolute(config)
  } else {
    cli::cli_abort("Config file not found in opensmile installation.")
  }
  config
}


# extract_opensmile() ----------------------------------------------------------

#' @export
extract_opensmile <- function(infile, aggfile, lldfile = NULL,
                              config = "misc/emo_large", tidy = TRUE) {

  stopifnot(
    is.character(infile), length(infile) == 1,
    is.character(aggfile), length(aggfile) == 1,
    is.character(config), length(config) == 1
  )
  
  stopifnot(
    is.null(lldfile) || (is.character(lldfile) && length(lldfile) == 1)
  )

  config <- check_config(config)

  if (!dir.exists(dirname(aggfile))) dir.create(dirname(aggfile), recursive = TRUE)
  
  arg <- paste0(
    '-C "', config, '"',
    ' -I "', infile, '"',
    ' -csvoutput "', aggfile, '"'
  )

  if (!is.null(lldfile)) {
    if (!dir.exists(dirname(lldfile))) dir.create(dirname(lldfile), recursive = TRUE)
    arg <- paste0(arg, ' -lldcsvoutput "', lldfile, '"')
  }

  out <- opensmile(arg)

  if (tidy == TRUE) {
    tidy_opensmile(aggfile)
    tidy_opensmile(lldfile)
  }

  out
}


# extract_opensmile_dir() -------------------------------------------------------------

#' @export
extract_opensmile_dir <- function(indir, aggdir, llddir = NULL, config = "misc/emo_large", 
                                  tidy = TRUE, recursive = FALSE, .progress = TRUE) {

  stopifnot(dir.exists(indir))

  infiles <- list.files(
    path = indir,
    pattern = "wav$",
    full.names = TRUE,
    recursive = recursive
  )

  aggfiles <- gsub(indir, aggdir, infiles)
  aggfiles <- gsub("wav", "csv", outfiles)

  if (!is.null(llddir)) {
    lldfiles <- gsub(indir, llddir, infiles)
    lldfiles <- gsub("wav", "csv", outfiles)
    furrr::future_pwalk(
      .l = data.frame(
        infile = infiles,
        aggfile = aggfiles,
        lldfile = lldfiles
      ),
      config = config,
      tidy = tidy,
      .f = extract_opensmile,
      .progress = .progress
    )
  } else {
    furrr::future_pwalk(
      .l = data.frame(
        infile = infiles,
        aggfile = aggfiles
      ),
      config = config,
      tidy = tidy,
      lldfile = NULL,
      .f = extract_opensmile,
      .progress = .progress
    )
  }
}


# check_opensmile() ------------------------------------------------------------

#' @export
check_opensmile <- function() {

  # Try to find the opensmile executable
  of <- find_opensmile()

  if (is.null(of)) {
    return(FALSE)
  }

  # Try to call the openface executable
  res <- try(opensmile('-h'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}


# tidy_opensmile() -------------------------------------------------------------

#' @export
tidy_opensmile <- function(infile) {
  df <- read.csv(file = infile, sep = ";", dec = ".")
  write.csv(df, file = infile, row.names = FALSE)
}
