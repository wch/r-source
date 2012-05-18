% File src/library/graphics/man/plot.table.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{plot.table}
\alias{plot.table}
\alias{lines.table}
\alias{points.table}
\title{Plot Methods for \code{table} Objects}
\description{
  This is a method of the generic \code{plot} function for
  (contingency) \code{\link{table}} objects.  Whereas for two- and more
  dimensional tables, a \code{\link{mosaicplot}} is drawn,
  one-dimensional ones are plotted as bars.
}
\usage{
\method{plot}{table}(x, type = "h", ylim = c(0, max(x)), lwd = 2,
     xlab = NULL, ylab = NULL, frame.plot = is.num, \dots)
\method{points}{table}(x, y = NULL, type = "h", lwd = 2, \dots)
\method{lines}{table}(x, y = NULL, type = "h", lwd = 2, \dots)
}
\arguments{
  \item{x}{a \code{\link{table}} (like) object.}
  \item{y}{Must be \code{NULL}: there to protect against incorrect calls.}
  \item{type}{plotting type.}
  \item{ylim}{range of y-axis.}
  \item{lwd}{line width for bars when \code{type = "h"} is used in the 1D case.}
  \item{xlab, ylab}{x- and y-axis labels.}
  \item{frame.plot}{logical indicating if a frame (\code{\link{box}})
    should be drawn in the 1D case.  Defaults to true when \code{x} has
    \code{\link{dimnames}} coerce-able to numbers.}
  \item{\dots}{further graphical arguments, see \code{\link{plot.default}}.
    \code{axes = FALSE} is accepted.}
}
\seealso{
  \code{\link{plot.factor}}, the \code{\link{plot}} method for factors.
}

\examples{
## 1-d tables
(Poiss.tab <- table(N = stats::rpois(200, lambda = 5)))
plot(Poiss.tab, main = "plot(table(rpois(200, lambda = 5)))")

plot(table(state.division))

## 4-D :
plot(Titanic, main ="plot(Titanic, main= *)")

}
\keyword{hplot}
\keyword{category}
