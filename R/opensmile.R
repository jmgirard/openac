
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
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run opensmile
  system2(find_opensmile(), args = arg, stdout = TRUE, stderr = TRUE)
}


# opensmile_configs() ----------------------------------------------------------

#' @export
opensmile_configs <- function() {
  # Find opensmile install directory
  fd <- dirname(find_opensmile())
  # Find all config files
  list.files(
    path = file.path(fd, "..", "config"),
    pattern = ".conf$",
    full.names = FALSE,
    recursive = TRUE
  )
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
  aggfiles <- gsub("wav", "csv", aggfiles)

  if (!is.null(llddir)) {
    lldfiles <- gsub(indir, llddir, infiles)
    lldfiles <- gsub("wav", "csv", lldfiles)
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

# opensmile_prepare_audio() -------------------------------------------------------------

#' Prepare an audio stream for analysis by opensmile
#' 
#' Import an audio or video file and export an audio file for acoustic analysis. Extract the audio 
#' stream specified by `stream` and then transcode it to a mono 16-bit PCM .wav file at 44.1kHz.
#' 
#' @param infile (character) The filepath of the audio or video file to import.
#' @param outfile (character) The filepath of the .wav file to create.
#' @param stream (numeric, default=0) The index of the audio stream to extract (ffmpeg uses zero-indexing so 0 is the first stream).
#' @return A character vector containing the output of ffmpeg.
#' @export
opensmile_prepare_audio <- function(infile, outfile, stream = 0) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_character(outfile, n = 1))
  stopifnot(rlang::is_integerish(stream, n = 1), stream >= 0)
  # Create outfile directory if needed
  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive = TRUE)
  # Construct ffmpeg command
  arg <- paste0(
    '-y -i "', infile, '" ',
    ' -map 0:a:', stream,
    ' -ar 44100', # set sample rate to 44.1kHz
    ' -ac 1', # set to mono audio (1 channel)
    ' -c:a pcm_s16le', # set to 16-bit PCM Little-Endian codec
    ' "', outfile, '"'
  )
  # Run ffmpeg command
  ffmpeg(arg)
}

# opensmile_prepare_audio_dir() -------------------------------------------------------------

#' Run opensmile_prepare_audio on multiple files in a directory
#' 
#' Find all media files with a specified extension in a specified directory and then
#' extract an audio file for acoustic analysis from each.
#' 
#' @param indir (string) What directory contains the input files?
#' @param inext (string) What file extension should we look for in `indir` (e.g., "mp4" or "mp3")?
#' @param outdir (string) What directory should the audio files be output to?
#' @param stream (number, default=0) Which audio stream to extract? This value is zero-indexed, so 0 is the first stream.
#' @param recursive (logical, default=FALSE) Should subdirectories of `indir` be included?
#' @param progress (logical, default=TRUE) Should a progress bar be created?
#' @return `NULL`
#' @export
opensmile_prepare_audio_dir <- function(indir, inext, outdir, stream = 0, 
                             recursive = FALSE, progress = TRUE) {

  # Validate input
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_character(inext, n = 1))
  stopifnot(rlang::is_character(outdir, n = 1))
  stopifnot(rlang::is_integerish(stream, n = 1), stream >= 0)
  stopifnot(rlang::is_logical(recursive, n = 1))
  stopifnot(rlang::is_logical(progress, n = 1))
  # Find input filenames
  infiles <- list.files(
    path = indir,
    pattern = paste0(inext, "$"),
    full.names = TRUE,
    recursive = recursive
  )
  # Construct output filenames
  outfiles <- gsub(indir, outdir, infiles)
  outfiles <- gsub(inext, "wav", outfiles)
  # Iterate extract_hifi() over infiles
  furrr::future_pwalk(
    .l = data.frame(
      infile = infiles,
      outfile = outfiles
    ),
    .f = opensmile_prepare_audio,
    stream = stream,
    .progress = progress
  )
}
