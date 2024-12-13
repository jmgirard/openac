% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffmpeg.R
\name{ffprobe}
\alias{ffprobe}
\title{Low-level access to the ffprobe command line interface}
\usage{
ffprobe(arg)
}
\arguments{
\item{arg}{A string including space-separated arguments to append to the
ffprobe command line call.}
}
\value{
A character vector containing the output of ffprobe.
}
\description{
Attempt to find and run ffprobe with the specified arguments.
}
\examples{
ffprobe('-version')
}
\references{
https://ffmpeg.org/ffprobe.html
}
