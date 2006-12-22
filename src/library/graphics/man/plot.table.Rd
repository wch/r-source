\name{plot.table}
\alias{plot.table}
\title{Plot Methods for `table' Objects}
\description{
  This is a method of the generic \code{plot} function for
  (contingency) \code{\link{table}} objects.  Whereas for two- and more
  dimensional tables, a \code{\link{mosaicplot}} is drawn,
  one-dimensional ones are plotted \dQuote{bar like}.
}
\usage{
%% Improvement ideas: see code definition, currently in ../R/plot.R
\method{plot}{table}(x, type = "h", ylim = c(0, max(x)), lwd = 2,
     xlab = NULL, ylab = NULL, frame.plot = is.num, \dots)
}
\arguments{
  \item{x}{a \code{\link{table}} (like) object.}
  \item{type}{plotting type.}
  \item{ylim}{range of y-axis.}
  \item{lwd}{line width for bars when \code{type = "h"} is used in the 1D case.}
  \item{xlab, ylab}{x- and y-axis labels.}
  \item{frame.plot}{logical indicating if a frame (\code{\link{box}})
    should be drawn in the 1D case.  Defaults to true when \code{x} has
    \code{\link{dimnames}} coerce-able to numbers.}
  \item{\dots}{further graphical arguments, see \code{\link{plot.default}}.}
}
\details{
  The current implementation (R 1.2) is somewhat experimental and will
  be improved and extended.
}
\seealso{\code{\link{plot.factor}}, the \code{\link{plot}} method for factors.}

\examples{
## 1-d tables
(Poiss.tab <- table(N = rpois(200, lam= 5)))
plot(Poiss.tab, main = "plot(table(rpois(200, lam=5)))")

plot(table(state.division))

## 4-D :
plot(Titanic, main ="plot(Titanic, main= *)")

}
\keyword{hplot}
\keyword{category}
