# opensmile --------------------------------------------------------------------

#' Low-level access to the opensmile command line interface
#'
#' Attempt to find and run opensmile with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   SMILEextract command line call.
#' @return A character vector containing the output of openface.
#' @references https://audeering.github.io/opensmile/
#' @aliases os
#' @export
#' @examples
#' opensmile('-h')
#'
opensmile <- function(arg) {
  # Validate input
  stopifnot(rlang::is_string(arg))
  # Run opensmile
  system2(find_opensmile(), args = arg, stdout = TRUE, stderr = TRUE)
}


# os ---------------------------------------------------------------------------

#' @rdname opensmile
#' @export
os <- opensmile


# os_list_configs --------------------------------------------------------------

#' List openSMILE configuration files
#'
#' Return a list of all configuration (.config) files found in the openSMILE
#' installation folder.
#'
#' @return A character vector containing the configuration files found.
#' @export
#' @examples
#' os_list_configs()
#'
os_list_configs <- function() {
  # Find opensmile install directory
  fd <- dirname(find_opensmile())
  # Find all config files
  configs <- list.files(
    path = file.path(fd, "..", "config"),
    pattern = ".conf$",
    full.names = FALSE,
    recursive = TRUE
  )
  # Strip away file extensions
  tools::file_path_sans_ext(configs)
}


# os_check_config --------------------------------------------------------------

#' @export
os_check_config <- function(config) {
  # Validate input
  stopifnot(rlang::is_string(config))
  # Strip away file extensions
  config_sans <- tools::file_path_sans_ext(config)
  configs_sans <- tools::file_path_sans_ext(os_list_configs())
  if (config_sans %in% configs_sans == FALSE) {
    cli::cli_abort("Config file not found in opensmile installation.")
  }
  # Get absolute path to config
  fd <- dirname(find_opensmile())
  config <- file.path(fd, "..", "config", paste0(config_sans, ".conf"))
  tools::file_path_as_absolute(config)
}


# os_check_audio ---------------------------------------------------------------

#' Check if an audio file is ready for analysis by openSMILE
#'
#' Check if an audio file has the proper format for openSMILE, i.e., the
#' pcm_s16le audio codec and 1 audio channel.
#'
#' @param infile A required string indicating the filepath of the audio file to
#'   check.
#' @param verbose An optional logical indicating whether to print warnings.
#' @return A logical indicating whether `infile` is ready for openSMILE
#' @export
os_check_audio <- function(infile, verbose = FALSE) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_bool(verbose))
  # Count streams
  streams <- ffp_count_streams(infile)
  # Create ffprobe command
  arg <- paste0(
    '-v error',
    ' -show_entries stream=codec_name,sample_rate,channels',
    ' -of default=noprint_wrappers=1:nokey=1',
    ' "', infile, '"'
  )
  # Run ffprobe command
  dat <- ffprobe(arg)
  # Check ffprobe output
  tests <- c(
    No_Video = streams["Video"] == 0,
    One_Stream = streams["Audio"] == 1,
    Right_Codec = dat[[1]] == "pcm_s16le",
    One_Channel = dat[[3]] == "1"
  )
  # If verbose, state the result
  if (verbose) {
    print(tests)
    if (dat[[2]] != "44100") {
      cli::cli_warn("A sampling rate of 44100 is recommended.")
    }
  }
  # Return single logical
  all(tests)
}


# os_prep_audio ----------------------------------------------------------------

#' Prepare an audio stream for analysis by opensmile
#'
#' Import an audio or video file and export an audio file for acoustic analysis.
#' Extract the audio stream specified by `stream` and then transcode it to a
#' mono (i.e., single channel) 16-bit PCM .wav file at 44.1kHz sampling rate.
#'
#' @param infile (character) What is the filepath of the audio or video file
#'   to import?
#' @param outfile (character) What is the filepath of the .wav file to create?
#' @param stream (numeric, default=0) The index of the audio stream to extract
#' (ffmpeg uses zero-indexing so 0 is the first stream).
#' @param overwrite Should outfile be overwritten if it already exists? It will
#'   be skipped otherwise. Defaults to TRUE.
#' @return A character vector containing the output of ffmpeg.
#' @export
#'
os_prep_audio <- function(infile, outfile, stream = 0, overwrite = TRUE) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_string(outfile))
  stopifnot(rlang::is_integerish(stream, n = 1), stream >= 0)
  stopifnot(rlang::is_bool(overwrite))
  # Return early if overwrite is TRUE and outfile exists
  if (overwrite == FALSE && file.exists(outfile)) {
    return("Skipped")
  }
  # Create outfile directory if needed
  if (!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }
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


# os_prep_audio_dir ------------------------------------------------------------

#' Run os_prep_audio() on multiple files in a directory
#'
#' Find all media files with a specified extension in a specified directory and
#' then extract an audio file for acoustic analysis from each.
#'
#' Can be optionally run in parallel by running \code{\link[future]{plan}()}
#' beforehand, e.g., by calling `plan("multisession", workers = 4)`.
#'
#' Can optionally output a progress bar by using
#' \code{\link[progressr]{handlers}()} beforehand, e.g., by calling
#' `handlers("cli"); handlers(global = TRUE)`.
#'
#' @param indir (string) What directory contains the input files?
#' @param inext (string) What file extension should be looked for in `indir`
#'   (e.g., "mp4" or "mp3")?
#' @param outdir (string) What directory should the audio files be output to?
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @inheritDotParams os_prep_audio stream overwrite
#' @return `NULL`
#' @export
#'
os_prep_audio_dir <- function(
  indir,
  inext,
  outdir,
  recursive = FALSE,
  ...
) {
  # Validate input
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(rlang::is_string(outdir))
  stopifnot(rlang::is_bool(recursive))
  inext <- gsub("\\.", "", inext)
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
  # Iterate os_prep_audio() over infiles
  p <- progressr::progressor(along = infiles)
  furrr::future_pwalk(
    .l = data.frame(
      infile = infiles,
      outfile = outfiles
    ),
    .f = function(infile, outfile) {
      os_prep_audio(infile, outfile, ...)
      p() # update progress
    }
  )
}


# os_extract -------------------------------------------------------------------

#' Extract opensmile features
#'
#' Extract openSMILE acoustic features from an audio file based on a config
#' file. Lower level descriptors (LLDs) will be calculated per frame and then
#' summarized into an aggregate (AGG) file.
#'
#' @param infile (character) What is the filepath for the input file to be
#' analyzed? The proper format can be created by `os_prep_audio()`.
#' @param wavfile (character, default=NULL) Either NULL or a string indicating
#' the path to save the prepared version of `infile` to (must end with '.wav').
#' If NULL, a temporary file will be created and later discarded.
#' @param aggfile (character, default=NULL) What is the filepath to write the
#' AGG output to? If `NULL`, the AGG output will not be saved. Note that either
#' `aggfile` or `lldfile` (or both) must be non-NULL.
#' @param lldfile (character, default=NULL) What is the filepath to write the
#' LLD output to? If `NULL`, the LLD output will not be saved. Note that either
#' `aggfile` or `lldfile` (or both) must be non-NULL.
#' @param config (character, default="misc/emo_large") Which configuration file
#' should be used to analyze `infile`? A list of available config files can be
#' generated using `os_list_configs()`.
#' @inheritDotParams os_prep_audio stream overwrite
#' @return A character vector including opensmile output.
#' @export
#'
os_extract <- function(
  infile,
  wavfile = NULL,
  aggfile = NULL,
  lldfile = NULL,
  config = "misc/emo_large",
  ...
) {
  # Input validation will be handled by subfunctions
  # Preallocate temp
  temp <- FALSE
  if (os_check_audio(infile) == FALSE) {
    # If no wavfile provided, create tempfile
    if (is.null(wavfile)) {
      temp <- TRUE
      wavfile <- tempfile(fileext = ".wav")
    }
    # Prepare audio stream as wavfile/tempfile
    x <- os_prep_audio(
      infile = infile,
      outfile = wavfile,
      ...
    )
  } else {
    wavfile <- infile
  }
  # Extract features from prepared audio file
  out <- os_extract_wav(
    infile = wavfile,
    aggfile = aggfile,
    lldfile = lldfile,
    config = config
  )
  # Clean up temporary file if created
  if (temp) unlink(wavfile)
  # Return the output from opensmile
  out
}


# os_extract_wav ------------------------------------------------------------

os_extract_wav <- function(
  infile,
  aggfile = NULL,
  lldfile = NULL,
  config = "misc/emo_large"
) {
  # Validate inputs
  stopifnot(file.exists(infile), os_check_audio(infile))
  stopifnot(is.null(aggfile) ||
   (rlang::is_string(aggfile) && tools::file_ext(aggfile) == "csv"))
  stopifnot(is.null(lldfile) ||
    (rlang::is_string(lldfile) && tools::file_ext(lldfile) == "csv"))
  config <- os_check_config(config)
  # Create output directories if necessary
  if (!is.null(aggfile) && !dir.exists(dirname(aggfile))) {
    dir.create(dirname(aggfile), recursive = TRUE)
  }
  if (!is.null(lldfile) && !dir.exists(dirname(lldfile))) {
    dir.create(dirname(lldfile), recursive = TRUE)
  }
  # Construct opensmile command
  arg <- paste0(
    '-C "', config, '"',
    ' -I "', infile, '"',
    ifelse(
      test = !is.null(aggfile),
      yes = paste0(' -csvoutput "', aggfile, '"'),
      no = ''
    ),
    ifelse(
      test = !is.null(lldfile),
      yes = paste0(' -lldcsvoutput "', lldfile, '"'),
      no = ''
    ),
    ' -instname "', basename(infile), '"'
  )
  # Run opensmile command
  out <- opensmile(arg)
  # Fix the output CSV files
  if (!is.null(aggfile)) {
    os_fix_csv(aggfile)
  }
  if (!is.null(lldfile)) {
    os_fix_csv(lldfile)
  }
  # Return opensmile output
  out
}


# os_extract_dir ---------------------------------------------------------------

#' Run os_extract() on multiple files in a directory
#'
#' Find all .wav files in a specified directory and then extract opensmile
#' features from each (according to `config`).
#'
#' Can be optionally run in parallel by running \code{\link[future]{plan}()}
#' beforehand, e.g., by calling `plan("multisession", workers = 4)`.
#'
#' Can optionally output a progress bar by using
#' \code{\link[progressr]{handlers}()} beforehand, e.g., by calling
#' `handlers("cli"); handlers(global = TRUE)`.
#'
#' @param indir (character) What directory contains the input .wav files?
#' @param inext (character) What file extension to look for in `indir`?
#' @param wavdir (character, default=NULL) What directory should the prepared
#' WAV audio files be saved to? If `NULL`, temporary WAV files will be created
#' and then discarded (if needed).
#' @param aggdir (character, default=NULL) What directory should the AGG output
#' files be saved to? If `NULL`, AGG files will not be output. Note that
#' `aggdir` or `llddir` (or both) must be non-NULL.
#' @param llddir (character, default=NULL) What directory should the LLD output
#' files be saved to? If `NULL`, LLD files will not be output. Note that
#' `aggdir` or `llddir` (or both) must be non-NULL.
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @inheritDotParams os_extract config
#' @inheritDotParams os_prep_audio stream overwrite
#' @return `NULL`
#' @export
#'
os_extract_dir <- function(
  indir,
  inext,
  wavdir = NULL,
  aggdir = NULL,
  llddir = NULL,
  recursive = FALSE,
  ...
) {
  # Validate inputs
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(is.null(wavdir) || rlang::is_string(wavdir))
  stopifnot(is.null(aggdir) || rlang::is_string(aggdir))
  stopifnot(is.null(llddir) || rlang::is_string(llddir))
  stopifnot(!is.null(aggdir) || !is.null(llddir))
  stopifnot(rlang::is_bool(recursive))
  extra_args <- list(...)
  # Find input filepaths
  infiles <- list.files(
    path = indir,
    pattern = paste0(inext, "$"),
    full.names = TRUE,
    recursive = recursive
  )
  # Construct iteration data frame
  df <- data.frame(infile = infiles)
  # If saving prepared WAV files...
  if (!is.null(wavdir)) {
    # Construct WAV output filepaths
    wavfiles <- gsub(indir, wavdir, infiles)
    wavfiles <- gsub(inext, "wav", wavfiles)
    # Add to iteration data frame
    df <- cbind(df, wavfile = wavfiles)
  }
  # If exporting AGG output...
  if (!is.null(aggdir)) {
    # Construct AGG output filepaths
    aggfiles <- gsub(indir, aggdir, infiles)
    aggfiles <- gsub(inext, "csv", aggfiles)
    # Add to iteration data frame
    df <- cbind(df, aggfile = aggfiles)
  }
  # If exporting LLD output...
  if (!is.null(llddir)) {
    # Construct LLD output filepaths
    lldfiles <- gsub(indir, llddir, infiles)
    lldfiles <- gsub(inext, "csv", lldfiles)
    # Add to iteration data frame
    df <- cbind(df, lldfile = lldfiles)
  }
  # Iterate os_extract() over infiles
  p <- progressr::progressor(along = infiles)
  furrr::future_pwalk(
    .l = df,
    .f = function(...) {
      do.call(
        what = os_extract,
        args = c(list(...), extra_args)
      )
      p() # update progress
    }
  )
}


# os_fix_csv -------------------------------------------------------------------

os_fix_csv <- function(infile) {
  # Validate input
  stopifnot(file.exists(infile))
  # Read in opensmile output in original format
  df <- read.csv(file = infile, sep = ";", dec = ".")
  # Write out opensmile output in traditional format
  write.csv(df, file = infile, row.names = FALSE)
}
