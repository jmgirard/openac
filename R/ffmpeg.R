
# ffmpeg ------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffmpeg command line call.
#' @return A character vector containing the output of ffmpeg.
#' @references https://ffmpeg.org/ffmpeg.html
#' @export
#' @examples 
#' ffmpeg('-version')
ffmpeg <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run ffmpeg
  system2(find_ffmpeg(), args = arg, stdout = TRUE, stderr = TRUE)
}

# ffprobe -----------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg (string) A string of space-separated arguments to append to the
#'   ffprobe command line call.
#' @return A character vector containing the output of ffprobe.
#' @references https://ffmpeg.org/ffprobe.html
#' @export
#' @examples 
#' ffprobe('-version')
ffprobe <- function(arg) {
  # Validate input
  stopifnot(rlang::is_character(arg, n = 1))
  # Run ffprobe
  system2(find_ffprobe(), args = arg, stdout = TRUE, stderr = TRUE)
}

# count_audio_streams -------------------------------------------------------

#' Count the audio streams in a media file
#' 
#' Use ffprobe to count the number of audio streams in a media file.
#' 
#' @param infile (string) The filepath to the media file to import.
#' @return An integer indicating the number of audio streams in `infile`.
#' @export
count_audio_streams <- function(infile) {
  # Validate input
  stopifnot(file.exists(infile))
  # Construct ffprobe command
  arg <- paste0(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "',
    infile,
    '"'
  )
  # Run ffprobe command
  out <- ffprobe(arg)
  # Count the number of streams
  length(out)
}

determine_type <- function(infile) {
  # Validate inputs
  stopifnot(file.exists(infile))
  # Check if there is a video stream
  arg <- paste0(
    '-v error',
    ' -select_streams v:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  vcheck <- length(ffprobe(arg)) > 0
  # Check if there is an audio stream
  arg2 <- paste0(
    '-v error',
    ' -select_streams a:0',
    ' -show_entries stream=codec_type',
    ' -of csv=p=0',
    ' "', infile, '"'
  )
  acheck <- length(ffprobe(arg2)) > 0
  # Construct output vector
  c(Video = vcheck, Audio = acheck)
}

# check_ffmpeg() ------------------------------------------------------------

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

# check_ffprobe() -----------------------------------------------------------

#' Check that ffprobe is accessible
#' 
#' Check that ffprobe is installed and accessible and working properly.
#' 
#' @return A logical indicating whether ffprobe is working (TRUE) or not (FALSE).
#' @export
#' @examples
#' check_ffprobe()
#' @export
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

# extract_hifi() -------------------------------------------------------------

#' Extract High Fidelity Audio for Acoustic Analysis
#' 
#' Import an audio or video file and export an audio file for acoustic analysis. Extract the audio 
#' stream specified by `stream` and then transcode it to a mono 16-bit PCM .wav file at 44.1kHz.
#' 
#' @param infile (character) The filepath of the audio or video file to import.
#' @param outfile (character) The filepath of the .wav file to create.
#' @param stream (numeric, default=0) The index of the audio stream to extract (ffmpeg uses zero-indexing so 0 is the first stream).
#' @return A character vector containing the output of ffmpeg.
#' @export
extract_hifi <- function(infile, outfile, stream = 0) {
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

# extract_hifi_dir() -------------------------------------------------------------

#' Run extract_hifi on multiple files in a directory
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
extract_hifi_dir <- function(indir, inext, outdir, stream = 0, 
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
    .f = extract_hifi,
    stream = stream,
    .progress = progress
  )
}
