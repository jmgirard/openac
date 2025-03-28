% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_whisper.R
\name{aw_prep_audio}
\alias{aw_prep_audio}
\title{Prepare an audio stream for analysis by Whisper}
\usage{
aw_prep_audio(infile, outfile, stream = 0, afilters = FALSE)
}
\arguments{
\item{infile}{A required string indicating the filepath to the input audio or
video file containing the stream to convert or extract.}

\item{outfile}{A required string indicating the filepath to the audio (.wav)
file to create containing only the specified audio stream from \code{infile}.}

\item{stream}{An optional number indicating the index of the audio stream in
\code{infile} to convert or extract. Note that ffmpeg uses zero-indexing so the
default of 0 is the first stream (default = 0).}

\item{afilters}{Should audio filters be used to try to improve audio quality?
(See Details.) Defaults to FALSE.}
}
\value{
A string containing the text output from ffmpeg.
}
\description{
If provided an audio file, convert the specified audio stream to the proper
format for Whisper. Or, if provided a video file, extract the specified audio
stream and convert it to the proper format for Whisper.
}
\details{
The audio filters applied when \code{afilters = TRUE} are normalizing loudness
(loudnorm), filtering to human speech frequencies (lowpass, highpass),
reducing noise in the frequency domain (afftdn), compressing dynamic range
(compand), dynamically normalizing volume (dynaudnorm), and boosting subtle
transient details (areverse, asubboost, areverse).
}
