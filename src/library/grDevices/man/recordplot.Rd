\name{recordPlot}
\alias{replayPlot}
\alias{recordPlot}
\alias{print.recordedplot}
\title{Record and Replay Plots}
\description{
  Functions to save the current plot in an \R variable, and to replay it.
}
\usage{
recordPlot()
replayPlot(x)
}
\arguments{
 \item{x}{A saved plot.}
}
\details{
  These functions record and replay the displaylist of the current
  graphics device.  The returned object is of class
  \code{"recordedplot"}, and \code{replayPlot} acts as a \code{print}
  method for that class.
}
\section{Warning}{
  The format of recorded plots may change between \R versions.
  Recorded plots should \strong{not} be used as a permanent
  storage format for \R plots.

  \R will always attempt to replay a recorded plot, but if the plot was 
  recorded with a different \R version then bad things may happen.
}
\value{
  \code{recordPlot} returns an object of class \code{"recordedplot"}.

  \code{replayPlot} has no return value.
}
\keyword{iplot}
