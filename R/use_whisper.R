# aw_check_audio ---------------------------------------------------------------

#' Check if an audio file is ready for analysis by Whisper
#'
#' Check if an audio file has the proper format for Whisper, i.e., the pcm_s16le
#' audio codec, a sampling rate of 16000, and 1 audio channel
#'
#' @param infile A required string indicating the filepath of the audio file to
#'   check.
#' @param verbose An optional logical indicating whether to print warnings.
#' @return A logical indicating whether `infile` is ready for whisper.
#' @export
aw_check_audio <- function(infile, verbose = FALSE) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_bool(verbose))
  # Count streams
  streams <- ffp_count_streams(infile)
  # Create ffprobe command
  arg <- paste0(
    '-v error',
    ' -select_streams a', 
    ' -show_entries stream=codec_name,sample_rate,channels',
    ' -of default=noprint_wrappers=1:nokey=1',
    ' "', infile, '"'
  )
  # Run ffprobe command
  dat <- ffprobe(arg)
  # Validate ffprobe output
  if (length(dat) < 3) {
    if (verbose) {
      cli::cli_warn(c(
        "!" = "ffprobe failed to retrieve audio metadata for {.file {infile}}",
        "i" = "This usually means the file has no audio streams or is corrupted.",
        "i" = "Returning {.val FALSE} so conversion is attempted."
      ))
    }
    return(FALSE)
  }
  # Check ffprobe output
  tests <- c(
    No_Video = streams[["Video"]] == 0,
    One_Stream = streams[["Audio"]] == 1,
    Right_Codec = dat[[1]] == "pcm_s16le",
    Sample_Rate = dat[[2]] == "16000",
    One_Channel = dat[[3]] == "1"
  )
  # If verbose, state the result
  if (verbose) {
    cli::cli_h2("Audio Check Results")
    cli::cli_ul(items = tests)
  }
  # Return single logical
  all(tests)
}


# aw_prep_audio ----------------------------------------------------------------

#' Prepare an audio stream for analysis by Whisper
#'
#' If provided an audio file, convert the specified audio stream to the proper
#' format for Whisper. Or, if provided a video file, extract the specified audio
#' stream and convert it to the proper format for Whisper.
#'
#' The audio filters applied when `afilters = TRUE` are normalizing loudness
#' (loudnorm), filtering to human speech frequencies (lowpass, highpass),
#' reducing noise in the frequency domain (afftdn), compressing dynamic range
#' (compand), dynamically normalizing volume (dynaudnorm), and boosting subtle
#' transient details (areverse, asubboost, areverse).
#'
#' @param infile A required string indicating the filepath to the input audio or
#'   video file containing the stream to convert or extract.
#' @param outfile A required string indicating the filepath to the audio (.wav)
#'   file to create containing only the specified audio stream from `infile`.
#' @param stream An optional number indicating the index of the audio stream in
#'   `infile` to convert or extract. Note that ffmpeg uses zero-indexing so the
#'   default of 0 is the first stream. Defaults to 0.
#' @param overwrite Should outfile be overwritten if it already exists? It will
#'   be silently skipped otherwise. Defaults to TRUE.
#' @param afilters Should audio filters be used to try to improve audio quality?
#'   (See Details.) Defaults to FALSE.
#' @return A string containing the text output from ffmpeg.
#' @export
aw_prep_audio <- function(
  infile,
  outfile,
  stream = 0,
  overwrite = TRUE,
  afilters = FALSE
) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_string(outfile))
  stopifnot(rlang::is_integerish(stream, n = 1), stream >= 0)
  stopifnot(rlang::is_bool(afilters))
  stopifnot(rlang::is_bool(overwrite))
  # Return early if overwrite is TRUE and outfile exists
  if (overwrite == FALSE && file.exists(outfile)) {
    return("Skipped")
  }
  # Check that the requested audio stream exists
  stopifnot((stream + 1) <= ffp_count_streams(infile)['Audio'])
  # Create output directory if necessary
  if (!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }
  # Construct audio filters if requested
  if (afilters) {
    afstring <- paste0(
      ' -af "',
      # Normalize loudness
      'loudnorm=I=-24:LRA=7:tp=-2,',
      # Filter to human speech frequencies
      'highpass=f=70,',
      'lowpass=f=14000,',
      # Reduce noise in frequency domain
      'afftdn=nf=-20,',
      # Compress dynamic range
      'compand=attacks=0:points=-80/-80|-50/-50|-20/-5|-5/-3:soft-knee=6,',
      # Dynamically normalize volume
      'dynaudnorm=p=0.7,',
      # Boost subtle transient details
      'areverse,',
      'asubboost,',
      'areverse"'
    )
  }
  # Construct ffmpeg command
  arg <- paste0(
    '-y -i "', infile, '"',
    ' -map 0:a:', stream,
    ifelse(test = afilters, yes = afstring, no = ''),
    ' -ar 16000', # set sample rate to 16kHz
    ' -ac 1', # set to mono audio (1 channel)
    ' -c:a pcm_s16le', # set to 16-bit PCM codec
    ' "', outfile, '"'
  )
  # Run ffmpeg command
  ffmpeg(arg)
}

# aw_prep_audio_dir ------------------------------------------------------------

#' Run aw_prep_audio() on multiple files in a directory
#'
#' Find all media files with a specified extension in a specified directory and
#' then extract an audio file for acoustic analysis from each.
#'
#' Can be optionally run in parallel by running \code{\link[future]{plan}()}
#' beforehand, e.g., by calling `plan("multisession", workers = 4)`.
#'
#' @param indir (string) What directory contains the input files?
#' @param inext (string) What file extension should be looked for in `indir`
#'   (e.g., "mp4" or "mp3")?
#' @param outdir (string) What directory should the audio files be output to?
#' @param recursive (logical, default = FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @param progress (string, default = "auto") Controls progress reporting.
#'   One of `"auto"`, `"on"`, or `"off"`.
#'   - `"auto"`: Emit `progressr` signals but do not force display; a progress
#'     bar appears only if the caller has enabled a handler (e.g.,
#'     `progressr::with_progress()` or a global handler).
#'   - `"on"`: Wraps the call in `progressr::with_progress()` so progress will
#'     render using any available handler.
#'   - `"off"`: Suppresses progress signals via `progressr::without_progress()`.
#' @inheritDotParams aw_prep_audio stream overwrite afilters
#' @return `NULL`
#' @export
#'
aw_prep_audio_dir <- function(
  indir,
  inext,
  outdir,
  recursive = FALSE,
  progress = c("auto", "on", "off"),
  ...
) {
  # Validate input
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(rlang::is_string(outdir))
  stopifnot(rlang::is_bool(recursive))
  progress <- match.arg(progress)
  # Drop leading extension dot if present
  inext <- gsub("^\\.", "", inext)
  # Find input filenames
  infiles <- list.files(
    path = indir,
    pattern = paste0("\\.", inext, "$"),
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  if (length(infiles) == 0L) {
    return(invisible(character()))
  }
  # Construct output filenames
  indir_abs   <- fs::path_abs(indir)
  infiles_abs <- fs::path_abs(infiles)
  outdir_abs  <- fs::path_abs(outdir)
  rel <- fs::path_rel(infiles_abs, start = indir_abs)
  if (any(startsWith(rel, ".."))) {
    cli::cli_abort("All input files must be located under 'indir'.")
  }
  outfiles <- fs::path(outdir_abs, rel)
  outfiles <- fs::path_ext_set(outfiles, "wav")
  fs::dir_create(fs::path_dir(outfiles))
  # Iterate os_prep_audio() over infiles
  run <- function() {
    p <- progressr::progressor(steps = length(infiles_abs))
    furrr::future_pwalk(
      .l = list(infile = infiles_abs, outfile = outfiles),
      .f = function(infile, outfile) {
        aw_prep_audio(infile, outfile, ...)
        p(message = basename(infile))
      }
    )
  }
  if (progress == "on") {
    progressr::with_progress(run())
  } else if (progress == "off") {
    progressr::without_progress(run())
  } else {
    run()
  }
  invisible(outfiles)
}


# aw_get_model -----------------------------------------------------------------

#' Get a Whisper model from a local or online source
#'
#' This function is an alias for `audio.whisper::whisper()`. Refer to
#' `audio.whisper::whisper()` for full documentation of available parameters
#' and usage details.
#'
#' @inheritParams audio.whisper::whisper
#' @seealso [audio.whisper::whisper()]
#' @return The result from `audio.whisper::whisper()`.
#' @export
aw_get_model <- function(...) {
  audio.whisper::whisper(...)
}


# aw_transcribe ----------------------------------------------------------------

#' Transcribe an audio stream using the specified Whisper model
#'
#' If provided a pre-prepared wav file, will directly transcribe it. But if
#' provided an unsupported audio format or video file, will extract and convert
#' the specified audio stream before transcribing it. Set `wavfile` if you want
#' to retain this conversion or leave it NULL to create and later discard a
#' temporary wav file.
#'
#' @param infile A required string indicating the file path to an audio or video
#'   file containing an audio stream to transcribe.
#' @param model A required model object produced by \code{\link{whisper}}.
#' @param language The language of the audio. Defaults to 'auto'. For a list of
#'   all languages the model can handle: see
#'   \code{\link[audio.whisper]{whisper_languages}}.
#' @param wavfile Either NULL or a string indicating the path to save the
#'   prepared version of `infile` to (must end with '.wav'). If NULL, a
#'   temporary file will be created and later discarded.
#' @param rdsfile Either NULL or a string indicating the path to save the full
#'   whisper output list object to (must end with '.rds').
#' @param csvfile Either NULL or a string indicating the path to save a
#'   human-readable version of the transcript to (must end with '.csv').
#' @param audio_args A list of optional arguments to forward to
#'   \code{\link{aw_prep_audio}}.
#' @param whisper_args A list of optional arguments to forward to
#'   \code{\link[audio.whisper]{predict.whisper}}.
#' @return A list object containing the full whisper output.
#' @export
aw_transcribe <- function(
  infile,
  model,
  language = "auto",
  wavfile = NULL,
  rdsfile = NULL,
  csvfile = NULL,
  audio_args = list(),
  whisper_args = list()
) {
  # Input validation will be handled by subfunctions
  # Preallocate temp
  temp <- FALSE
  if (aw_check_audio(infile) == FALSE) {
    # If no wavfile provided, create tempfile
    if (is.null(wavfile)) {
      temp <- TRUE
      wavfile <- tempfile(fileext = ".wav")
    }
    # Prepare audio stream as wavfile/tempfile
    do.call(
      what = aw_prep_audio,
      args = c(
        list(
          infile = infile,
          outfile = wavfile
        ),
        audio_args
      )
    )
  } else {
    wavfile <- infile
  }
  # Transcribe prepared audio file
  out <- aw_transcribe_wav(
    infile = wavfile,
    model = model,
    language = language,
    rdsfile = rdsfile,
    csvfile = csvfile,
    whisper_args = whisper_args
  )
  # Clean up temporary file if created
  if (temp) unlink(wavfile)
  # Return the output from whisper
  out
}


# aw_transcribe_wav ------------------------------------------------------------

aw_transcribe_wav <- function(
  infile,
  model,
  language = "auto",
  rdsfile = NULL,
  csvfile = NULL,
  whisper_args = list()
) {
  # Validate inputs
  stopifnot(file.exists(infile), aw_check_audio(infile))
  stopifnot(class(model) == "whisper")
  stopifnot(rlang::is_string(language))
  stopifnot(is.null(rdsfile) ||
    (rlang::is_string(rdsfile) && tools::file_ext(rdsfile) == "rds"))
  stopifnot(is.null(csvfile) ||
    (rlang::is_string(csvfile) && tools::file_ext(csvfile) == "csv"))
  stopifnot(is.list(whisper_args))
  # Run whisper
  out <- do.call(
    what = predict,
    args = c(
      list(
        object = model,
        newdata = infile,
        type = "transcribe",
        language = language,
        trace = FALSE
      ),
      whisper_args
    )
  )
  # Create RDS output if requested
  if (!is.null(rdsfile)) {
    if (!dir.exists(dirname(rdsfile))) {
      dir.create(dirname(rdsfile), recursive = TRUE)
    }
    saveRDS(out, file = rdsfile)
  }
  # Create CSV output if requested
  if (!is.null(csvfile)) {
    if (!dir.exists(dirname(csvfile))) {
      dir.create(dirname(csvfile), recursive = TRUE)
    }
    write.csv(out$data, file = csvfile, row.names = FALSE)
  }
  # Return whisper output
  out
}


# aw_transcribe_dir ------------------------------------------------------------

#' Transcribe multiple media files with Whisper
#'
#' Find all files in a specified directory with a specified extension and then
#' apply \code{\link{aw_transcribe}} to each to transcribe them. If the input files are
#' not in the format expected by Whisper, they will be converted first.
#'
#' Can optionally output a progress bar by using
#' \code{\link[progressr]{handlers}}, e.g., by calling
#' `handlers("cli"); handlers(global = TRUE)` before this code.
#'
#' Cannot be run in parallel due to using the GPU.
#'
#' @param indir (character) What directory contains the input files?
#' @param inext (character) What file extension should be looked for in `indir`
#'   (e.g., "mp4" or "mp3")?
#' @param wavdir (character, default=NULL) What directory should the prepared
#'   WAV files be saved to? If `NULL`, temporary WAV files will be created and
#'   later discarded.
#' @param rdsdir (character, default=NULL) What directory should the RDS output
#'   files be saved to? If `NULL`, RDS files will not be output.
#' @param csvdir (character, default=NULL) What directory should the CSV output
#'   files be saved to? If `NULL`, CSV files will not be output.
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @param progress (string, default = "auto") Controls progress reporting.
#'   One of `"auto"`, `"on"`, or `"off"`.
#'   - `"auto"`: Emit `progressr` signals but do not force display; a progress
#'     bar appears only if the caller has enabled a handler (e.g.,
#'     `progressr::with_progress()` or a global handler).
#'   - `"on"`: Wraps the call in `progressr::with_progress()` so progress will
#'     render using any available handler.
#'   - `"off"`: Suppresses progress signals via `progressr::without_progress()`.
#' @inheritDotParams aw_transcribe model language audio_args whisper_args
#' @return (Invisibly) the character vector of input files processed.
#' @export
#'
aw_transcribe_dir <- function(
  indir,
  inext,
  wavdir = NULL,
  rdsdir = NULL,
  csvdir = NULL,
  recursive = FALSE,
  progress = c("auto", "on", "off"),
  ...
) {
  # Validate inputs
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_string(inext))
  stopifnot(is.null(wavdir) || rlang::is_string(wavdir))
  stopifnot(is.null(rdsdir) || rlang::is_string(rdsdir))
  stopifnot(is.null(csvdir) || rlang::is_string(csvdir))
  stopifnot(rlang::is_bool(recursive))
  progress <- match.arg(progress)
  extra_args <- list(...)
  # Find input filepaths
  inext <- sub("^\\.", "", inext)
  infiles <- list.files(
    path = indir,
    pattern = paste0("\\.", inext, "$"),
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  if (length(infiles) == 0L) return(invisible(character()))
  # Normalize paths
  indir_abs <- fs::path_abs(indir)
  infiles_abs <- fs::path_abs(infiles)
  rel <- fs::path_rel(infiles_abs, start = indir_abs)
  if (any(startsWith(rel, ".."))) {
    stop("All input files must be located under 'indir'.")
  }
  # Build iteration data frame
  df <- data.frame(infile = infiles_abs, stringsAsFactors = FALSE)
  # WAV outputs
  if (!is.null(wavdir)) {
    wavdir_abs <- fs::path_abs(wavdir)
    wavfiles <- fs::path(wavdir_abs, rel)
    wavfiles <- fs::path_ext_set(wavfiles, "wav")
    fs::dir_create(fs::path_dir(wavfiles))
    df$wavfile <- wavfiles
  }
  # RDS outputs
  if (!is.null(rdsdir)) {
    rdsdir_abs <- fs::path_abs(rdsdir)
    rdsfiles <- fs::path(rdsdir_abs, rel)
    rdsfiles <- fs::path_ext_set(rdsfiles, "rds")
    fs::dir_create(fs::path_dir(rdsfiles))
    df$rdsfile <- rdsfiles
  }
  # CSV outputs
  if (!is.null(csvdir)) {
    csvdir_abs <- fs::path_abs(csvdir)
    csvfiles <- fs::path(csvdir_abs, rel)
    csvfiles <- fs::path_ext_set(csvfiles, "csv")
    fs::dir_create(fs::path_dir(csvfiles))
    df$csvfile <- csvfiles
  }
  # Work runner
  run <- function() {
    p <- progressr::progressor(steps = nrow(df))
    purrr::pwalk(
      .l = df,
      .f = function(...) {
        do.call(what = aw_transcribe, args = c(list(...), extra_args))
        p(message = basename(list(...)$infile))
      }
    )
  }
  if (progress == "on") {
    progressr::with_progress(run())
  } else if (progress == "off") {
    progressr::without_progress(run())
  } else {
    run()
  }
  invisible(infiles_abs)
}
