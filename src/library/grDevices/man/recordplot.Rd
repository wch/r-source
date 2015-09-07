% File src/library/grDevices/man/recordplot.Rd
% Part of the R package, https://www.R-project.org
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
  The format of recorded plots may change between \R versions, so
  recorded plots should \strong{not} be used as a permanent
  storage format for \R plots.
  
  As of \R 3.3.0, it is possible (again)
  to replay a plot from another \R session using, for example,
  \code{\link{saveRDS}} and \code{\link{readRDS}}.
  It is even possible to replay a plot from another \R version,
  however, this will produce warnings, may produce errors,
  or something worse.
}
\section{Note}{
  Replay of a recorded plot may not produce the correct result
  (or may just fail) if the display list contains a call to
  \code{\link{recordGraphics}} which in turn contains an expression
  that calls code from a non-base package other than \pkg{graphics}
  or \pkg{grid}.  The most well-known example of this is a plot
  drawn with the package \pkg{ggplot2}.  The solution is to load
  the relevant package(s) before replaying the recorded plot.
}
\value{
  \code{recordPlot} returns an object of class \code{"recordedplot"}.

  \code{replayPlot} has no return value.
}
\seealso{
  The displaylist can be turned on and off using \code{\link{dev.control}}.
  Initially recording is on for screen devices, and off for print devices.
}
\keyword{iplot}
