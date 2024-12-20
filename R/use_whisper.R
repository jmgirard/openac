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
  stopifnot(rlang::is_logical(verbose))
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
    Sample_Rate = dat[[2]] == "16000",
    One_Channel = dat[[3]] == "1"
  )
  # If verbose, state the result
  if (verbose) print(tests)
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
#' @param infile A required string indicating the filepath to the input audio or
#'   video file containing the stream to convert or extract.
#' @param outfile A required string indicating the filepath to the audio (.wav)
#'   file to create containing only the specified audio stream from `infile`.
#' @param stream An optional number indicating the index of the audio stream in
#'   `infile` to convert or extract. Note that ffmpeg uses zero-indexing so the
#'   default of 0 is the first stream (default = 0).
#' @return A string containing the text output from ffmpeg.
#' @export
aw_prep_audio <- function(infile, outfile, stream = 0) {
  # Validate input
  stopifnot(file.exists(infile))
  stopifnot(rlang::is_character(outfile, n = 1))
  stopifnot(rlang::is_integerish(stream, n = 1), stream >= 0)
  # Check that the requested audio stream exists
  stopifnot((stream + 1) <= ffp_count_streams(infile)['Audio'])
  # Create output directory if necessary
  if (!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }
  # Construct ffmpeg command
  arg <- paste0(
    '-y -i "', infile, '" ',
    ' -map 0:a:', stream,
    ' -ar 16000', # set sample rate to 16kHz
    ' -ac 1', # set to mono audio (1 channel)
    ' -c:a pcm_s16le', # set to 16-bit PCM codec
    ' "', outfile, '"'
  )
  # Run ffmpeg command
  ffmpeg(arg)
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
#' @param model A required model object produced by `whisper()`.
#' @param language The language of the audio. Defaults to 'auto'. For a list of
#'   all languages the model can handle: see
#'   `audio.whisper::whisper_languages()`.
#' @param stream An optional number indicating the index of the audio stream in
#'   `infile` to convert or extract. Note that ffmpeg uses zero-indexing so the
#'   default of 0 is the first stream (default = 0).
#' @param wavfile Either NULL or a string indicating the path to save the
#'   prepared version of `infile` to (must end with '.wav'). If NULL, a
#'   temporary file will be created and later discarded.
#' @param rdsfile Either NULL or a string indicating the path to save the full
#'   whisper output list object to (must end with '.rds').
#' @param csvfile Either NULL or a string indicating the path to save a
#'   human-readable version of the transcript to (must end with '.csv').
#' @param ... Optional arguments to forward to
#'   `audio.whisper:::predict.whisper()`.
#' @return A list object containing the full whisper output.
#' @export
aw_transcribe <- function(
  infile,
  model,
  language = "auto",
  stream = 0,
  wavfile = NULL,
  rdsfile = NULL,
  csvfile = NULL,
  ...
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
    x <- aw_prep_audio(
      infile = infile,
      outfile = wavfile,
      stream = stream
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
    ...
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
  ...
) {
  # Validate inputs
  stopifnot(file.exists(infile), aw_check_audio(infile))
  stopifnot(class(model) == "whisper")
  stopifnot(rlang::is_character(language, n = 1))
  stopifnot(is.null(rdsfile) || 
    (rlang::is_character(rdsfile, n = 1) && tools::file_ext(rdsfile) == "rds"))
  stopifnot(is.null(csvfile) || 
    (rlang::is_character(csvfile, n = 1) && tools::file_ext(csvfile) == "csv"))
  # Run whisper
  out <- predict(
    object = model,
    newdata = infile,
    type = "transcribe",
    language = language,
    trace = FALSE,
    ...
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

#' Run aw_transcribe() on multiple files in a directory
#' 
#' Find all files in a specified directory with a specified extension and then 
#' transcribe each using whisper. Can optionally be run in parallel
#' by using `plan()` beforehand; however, whisper is much more computationally 
#' intensive than the other programs (especially when using CUDA) and thus 
#' trying to run this in parallel could easily overwhelm your computer.
#' 
#' @param indir (character) What directory contains the input files?
#' @param inext (character) What file extension should be looked for in `indir` 
#'   (e.g., "mp4" or "mp3")?
#' @param rdsdir (character, default=NULL) What directory should the RDS output
#'   files be saved to? If `NULL`, RDS files will not be output.
#' @param csvdir (character, default=NULL) What directory should the CSV output
#'   files be saved to? If `NULL`, CSV files will not be output.
#' @inheritParams aw_transcribe
#' @param recursive (logical, default=FALSE) Should files in subdirectories
#'  within `indir` be included?
#' @param progress (logical, default=TRUE) Should a progress bar be shown?
#' @return A list object containing the whisper output for each input file.
#' @export
#' 
aw_transcribe_dir <- function(
  indir, 
  inext, 
  rdsdir = NULL, 
  csvdir = NULL, 
  model, 
  language = "auto", 
  ..., 
  recursive = FALSE, 
  progress = TRUE
) {
  # Validate inputs
  stopifnot(dir.exists(indir))
  stopifnot(rlang::is_character(inext, n = 1))
  stopifnot(is.null(rdsdir) || rlang::is_character(rdsdir, n = 1))
  stopifnot(is.null(csvdir) || rlang::is_character(csvdir, n = 1))
  stopifnot(rlang::is_logical(recursive, n = 1))
  stopifnot(rlang::is_logical(progress, n = 1))
  # Find input filepaths
  infiles <- list.files(
    path = indir,
    pattern = paste0(inext, "$"),
    full.names = TRUE,
    recursive = recursive
  )
  # Construct iteration data frame
  df <- data.frame(infile = infiles)
  # If exporting RDS output...
  if (!is.null(rdsdir)) {
    # Construct RDS output filepaths
    rdsfiles <- gsub(indir, rdsdir, infiles)
    rdsfiles <- gsub(inext, "rds", rdsfiles)
    # Add to iteration data frame
    df <- cbind(df, rdsfile = rdsfiles)
  }
  # If exporting CSV output...
  if (!is.null(csvdir)) {
    # Construct CSV output filepaths
    csvfiles <- gsub(indir, csvdir, infiles)
    csvfiles <- gsub(inext, "csv", csvfiles)
    # Add to iteration data frame
    df <- cbind(df, csvfile = csvfiles)
  }
  # Iterate aw_transcribe() over infiles
  furrr::future_pwalk(
    .l = df,
    .f = aw_transcribe,
    model = model,
    language = language,
    ...,
    .progress = progress
  )
}

