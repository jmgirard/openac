% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_whisper.R
\name{aw_transcribe}
\alias{aw_transcribe}
\title{Transcribe an audio stream using the specified Whisper model}
\usage{
aw_transcribe(
  infile,
  model,
  language = "auto",
  wavfile = NULL,
  rdsfile = NULL,
  csvfile = NULL,
  audio_args = list(),
  whisper_args = list()
)
}
\arguments{
\item{infile}{A required string indicating the file path to an audio or video
file containing an audio stream to transcribe.}

\item{model}{A required model object produced by \code{\link{whisper}}.}

\item{language}{The language of the audio. Defaults to 'auto'. For a list of
all languages the model can handle: see
\code{\link[audio.whisper]{whisper_languages}}.}

\item{wavfile}{Either NULL or a string indicating the path to save the
prepared version of \code{infile} to (must end with '.wav'). If NULL, a
temporary file will be created and later discarded.}

\item{rdsfile}{Either NULL or a string indicating the path to save the full
whisper output list object to (must end with '.rds').}

\item{csvfile}{Either NULL or a string indicating the path to save a
human-readable version of the transcript to (must end with '.csv').}

\item{audio_args}{A list of optional arguments to forward to
\code{\link{aw_prep_audio}}.}

\item{whisper_args}{A list of optional arguments to forward to
\code{\link[audio.whisper]{predict.whisper}}.}
}
\value{
A list object containing the full whisper output.
}
\description{
If provided a pre-prepared wav file, will directly transcribe it. But if
provided an unsupported audio format or video file, will extract and convert
the specified audio stream before transcribing it. Set \code{wavfile} if you want
to retain this conversion or leave it NULL to create and later discard a
temporary wav file.
}
