% File src/library/graphics/man/box.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{box}
\title{Draw a Box around a Plot}
\description{
  This function draws a box around the current plot in the given color
  and linetype.  The \code{bty} parameter determines
  the type of box drawn.  See \code{\link{par}} for details.
}
\usage{
box(which = "plot", lty = "solid", \dots)
}
\alias{box}
\arguments{
\item{which}{character, one of \code{"plot"}, \code{"figure"},
  \code{"inner"} and \code{"outer"}.}
\item{lty}{line type of the box.}
\item{\dots}{further \link{graphical parameters}, such as \code{bty},
  \code{col}, or \code{lwd}, see \code{\link{par}}.  Note that
  \code{xpd} is not accepted as clipping is always to the device
  region.}
}
\details{
  The choice of colour is complicated.  If \code{col} was supplied
  and is not \code{NA}, it is used.  Otherwise, if \code{fg} was supplied
  and is not \code{NA}, it is used.  The final default is \code{par("col")}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
\code{\link{rect}} for drawing of arbitrary rectangles.
}
\examples{
plot(1:7, abs(stats::rnorm(7)), type = "h", axes = FALSE)
axis(1, at = 1:7, labels = letters[1:7])
box(lty = '1373', col = 'red')
}
\keyword{aplot}
