\name{lines}
\alias{lines}
\alias{lines.default}
\title{Add Connected Line Segments to a Plot}
\description{
  A generic function taking coordinates given in various ways and
  joining the corresponding points with line segments.
}
\usage{
lines(x, \dots)

\method{lines}{default}(x, y = NULL, type = "l", col = par("col"),
      lty = par("lty"), \dots)
}
\arguments{
  \item{x, y}{coordinate vectors of points to join.}
  \item{type}{character indicating the type of plotting; actually any of
    the \code{type}s as in \code{\link{plot}}.}
  \item{col}{color to use. This can be vector of length greater than
    one, but only the first value will be used.}
  \item{lty}{line type to use.}
  \item{\dots}{Further graphical parameters (see \code{\link{par}}) may
    also be supplied as arguments, particularly, line type, \code{lty} and
    line width, \code{lwd}.}
}
\details{
  The coordinates can be passed to \code{lines} in a plotting structure
  (a list with \code{x} and \code{y} components), a time series,
  etc. See \code{\link{xy.coords}}.

  The coordinates can contain \code{NA} values. If a point contains
  \code{NA} in either its \code{x} or \code{y} value, it is omitted from
  the plot, and lines are not drawn to or from such points.  Thus
  missing values can be used to achieve breaks in lines.

  For \code{type = "h"}, \code{col} can be a vector and will be recycled
  as needed.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{points}}, particularly for \code{type \%in\% c("p","b","o")},
  \code{\link{plot}},
  and the underlying \dQuote{primitive} \code{\link{plot.xy}}.

  \code{\link{par}} for how to specify colors.
}
\examples{
# draw a smooth line through a scatter plot
plot(cars, main="Stopping Distance versus Speed")
lines(lowess(cars))
}
\keyword{aplot}
