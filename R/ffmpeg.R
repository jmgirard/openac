
# ffmpeg ------------------------------------------------------------------

#' @export
ffmpeg <- function(command) {
  stopifnot(is.character(command), length(command) == 1)
  out <- system(paste0('"', find_ffmpeg(), '" ', command), intern = TRUE)
  out
}

# ffprobe -----------------------------------------------------------------

#' @export
ffprobe <- function(command) {
  stopifnot(is.character(command) && length(command) == 1)
  system(paste0('"', find_ffprobe(), '" ', command), intern = TRUE)
}

# extract_wav() ---------------------------------------------------------

#' Extract audio for Whisper
#'
#' Extract an audio stream from a video file and transcode it for Whisper.
#'
#' @param infile A required string indicating the filepath of the video file.
#' @param outfile A required string indicating the filepath of the audio file to
#'   create.
#' @param stream An optional nonnegative integer indicating which audio stream
#'   to extract. Note that ffmpeg uses zero-indexing, so the first stream is 0.
#' @param check An optional logical indicating whether to check if `stream` is
#'   possible.
#' @param options An optional string indicating additional options to include in
#'   the ffmpeg command (after "-i").
#' @return A string containing the output from ffmpeg.
#' @export
extract_wav <- function(infile,
                        outfile,
                        stream = 0,
                        check = TRUE,
                        options = "") {

  stopifnot(is.character(infile), length(infile) == 1, file.exists(infile))
  stopifnot(is.character(outfile), length(outfile) == 1)
  stopifnot(is.numeric(stream) && length(stream) == 1)
  stopifnot(stream >= 0 && ceiling(stream) == floor(stream))
  stopifnot(is.logical(check) && length(check) == 1)
  stopifnot(is.character(options), length(options) == 1)

  if (check) {
    stopifnot((stream + 1) <= count_audio_streams(infile))
  }

  command <- paste0(
    '-i "', infile, '" ',
    options,
    ' -map 0:a:', stream,
    ' -ar 16000', # set sample rate to 16kHz
    ' -ac 1', # set to mono audio (1 channel)
    ' -c:a pcm_s16le', # set to 16-bit PCM codec
    ' "', outfile, '"'
  )
  ffmpeg(command)
}

#' @export
count_audio_streams <- function(infile) {
  command <- paste0(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "',
    infile,
    '"'
  )
  out <- ffprobe(command)
  length(out)
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
