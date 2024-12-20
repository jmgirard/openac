
# ffmpeg ------------------------------------------------------------------

#' Low-level access to the ffmpeg command line interface
#'
#' Attempt to find and run ffmpeg with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   ffmpeg command line call.
#' @return A character vector containing the output of ffmpeg.
#' @references https://ffmpeg.org/ffmpeg.html
#' @export
#' @examples
#' ffmpeg('-version')
ffmpeg <- function(arg) {
  stopifnot(is.character(arg), length(arg) == 1)
  system2(find_ffmpeg(), args = arg, stdout = TRUE, stderr = TRUE)
}

# ffprobe -----------------------------------------------------------------

#' Low-level access to the ffprobe command line interface
#'
#' Attempt to find and run ffprobe with the specified arguments.
#'
#' @param arg A string including space-separated arguments to append to the
#'   ffprobe command line call.
#' @return A character vector containing the output of ffprobe.
#' @references https://ffmpeg.org/ffprobe.html
#' @export
#' @examples
#' ffprobe('-version')
ffprobe <- function(arg) {
  stopifnot(is.character(arg), length(arg) == 1)
  system2(find_ffprobe(), args = arg, stdout = TRUE, stderr = TRUE)
}

count_audio_streams <- function(infile) {
  arg <- paste0(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "',
    infile,
    '"'
  )
  out <- ffprobe(arg)
  length(out)
}

determine_type <- function(infile) {

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

  c(Video = vcheck, Audio = acheck)
}

# check_ffmpeg() ------------------------------------------------------------

#' @export
check_ffmpeg <- function() {

  # Try to find the ffmpeg executable
  ffm <- find_ffmpeg()

  if (is.null(ffm)) {
    return(FALSE)
  }

  # Try to call the ffmpeg executable
  res <- try(ffmpeg('-version'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
}

# check_ffprobe() -----------------------------------------------------------

#' @export
check_ffprobe <- function() {

  # Try to find the ffprobe executable
  ffp <- find_ffprobe()

  if (is.null(ffp)) {
    return(FALSE)
  }

  # Try to call the ffprobe executable
  res <- try(ffprobe('-version'), silent = TRUE)

  if(inherits(res, "try-error")) {
    return(FALSE)
  }

  TRUE
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
  stopifnot(file.exists(infile))
  stopifnot(is.numeric(stream), length(stream) == 1, stream >= 0, floor(stream) == ceiling(stream))
  stopifnot(is.character(outfile), length(outfile) == 1)

  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive = TRUE)
  
  arg <- paste0(
    '-y -i "', infile, '" ',
    ' -map 0:a:', stream,
    ' -ar 44100', # set sample rate to 44.1kHz
    ' -ac 1', # set to mono audio (1 channel)
    ' -c:a pcm_s16le', # set to 16-bit PCM Little-Endian codec
    ' "', outfile, '"'
  )
  ffmpeg(arg)
}

# extract_hifi_dir() -------------------------------------------------------------

#' @export
extract_hifi_dir <- function(indir, inext, outdir, stream = 0, recursive = FALSE, .progress = TRUE) {

  stopifnot(dir.exists(indir))

  infiles <- list.files(
    path = indir,
    pattern = paste0(inext, "$"),
    full.names = TRUE,
    recursive = recursive
  )

  outfiles <- gsub(indir, outdir, infiles)
  outfiles <- gsub(inext, "wav", outfiles)
 
  furrr::future_pwalk(
    .l = data.frame(
      infile = infiles,
      outfile = outfiles,
      stream = stream
    ),
    .f = extract_hifi,
    .progress = .progress
  )
}
