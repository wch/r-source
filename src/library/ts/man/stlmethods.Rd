\name{stlmethods}
\alias{print.stl}
\alias{summary.stl}
\alias{plot.stl}
\title{Methods for STL Objects}
\description{
  Methods for objects of class \code{stl}, typically the result of
  \code{\link{stl}}.  The \code{plot} method does a multiple figure plot
  with some flexibility.
}
\usage{
\method{plot}{stl}(x, labels = colnames(X),
         set.pars = list(mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0),
                         tck = -0.01, mfrow = c(nplot, 1)),
         main = NULL, range.bars = TRUE, \dots)
\method{print}{stl}(x, \dots)
\method{summary}{stl}(object, digits = getOption("digits"), \dots)
}
\arguments{
  \item{x, object}{\code{\link{stl}} object.}
  \item{labels}{character of length 4 giving the names of the component
    time-series.}
  \item{set.pars}{settings for \code{\link{par}(.)} when setting up the plot.}
  \item{main}{plot main title.}
  \item{range.bars}{logical indicating if each plot should have a bar at
    its right side which are of equal heights in user coordinates.}
  \item{digits}{significant figures used in printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\seealso{\code{\link{plot.ts}} and \code{\link{stl}}, particularly for
  examples.}
\keyword{ts}
