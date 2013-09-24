% File src/library/grDevices/man/recordplot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

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

  The returned object is stored as a pairlist, but the usual methods for
  examining \R objects such as \code{\link{deparse}} and
  \code{\link{str}} are liable to mislead.
}
\section{Warning}{
  The format of recorded plots may change between \R versions.
  Recorded plots should \strong{not} be used as a permanent
  storage format for \R plots.  There were extensive changes in \R
  3.0.0, and now only plots from the current session can be replayed.

  \R will always attempt to replay a recorded plot, but if the plot was
  recorded with a different \R session then bad things may happen.
}
\value{
  \code{recordPlot} returns an object of class \code{"recordedplot"}.

  \code{replayPlot} has no return value.
}
\keyword{iplot}
