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

  The format of recorded plots was changed in \R 1.4.0: plots saved in
  earlier versions can still be replayed.
}
\value{
  \code{recordPlot} returns an object of class \code{"recordedplot"}, a
  list with components:
  \item{displaylist}{The saved display list, as a pairlist.}
  \item{gpar}{The graphics state, as an integer vector.}

  \code{replayPlot} has no return value.
}
\keyword{iplot}
