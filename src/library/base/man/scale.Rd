% File src/library/base/man/scale.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{scale}
\alias{scale}
\alias{scale.default}
\title{Scaling and Centering of Matrix-like Objects}
\description{
  \code{scale} is generic function whose default method centers and/or
  scales the columns of a numeric matrix.
}
\usage{
scale(x, center = TRUE, scale = TRUE)
}
\arguments{
  \item{x}{a numeric matrix(like object).}
  \item{center}{either a logical value or a numeric vector of length
    equal to the number of columns of \code{x}.}
  \item{scale}{either a logical value or a numeric vector of length
    equal to the number of columns of \code{x}.}
}
\value{
  For \code{scale.default}, the centered, scaled matrix.  The numeric
  centering and scalings used (if any) are returned as attributes
  \code{"scaled:center"} and \code{"scaled:scale"}
}
\details{
  The value of \code{center} determines how column centering is
  performed.  If \code{center} is a numeric vector with length equal to
  the number of columns of \code{x}, then each column of \code{x} has
  the corresponding value from \code{center} subtracted from it.  If
  \code{center} is \code{TRUE} then centering is done by subtracting the
  column means (omitting \code{NA}s) of \code{x} from their
  corresponding columns, and if \code{center} is \code{FALSE}, no
  centering is done.

  The value of \code{scale} determines how column scaling is performed
  (after centering).  If \code{scale} is a numeric vector with length
  equal to the number of columns of \code{x}, then each column of
  \code{x} is divided by the corresponding value from \code{scale}.
  If \code{scale} is \code{TRUE} then scaling is done by dividing the
  (centered) columns of \code{x} by their standard deviations if
  \code{center} is \code{TRUE}, and the root mean square otherwise.
  If \code{scale} is \code{FALSE}, no scaling is done.

  The root-mean-square for a (possibly centered) column is defined as
  \eqn{\sqrt{\sum(x^2)/(n-1)}}{sqrt(sum(x^2)/(n-1))}, where \eqn{x} is
  a vector of the non-missing values and \eqn{n} is the number of
  non-missing values.  In the case \code{center = TRUE}, this is the
  same as the standard deviation, but in general it is not.  (To scale
  by the standard deviations without centering, use
  \code{scale(x, center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE))}.)
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{sweep}} which allows centering (and scaling) with
  arbitrary statistics.

  For working with the scale of a plot, see \code{\link{par}}.
}
\examples{
require(stats)
x <- matrix(1:10, ncol = 2)
(centered.x <- scale(x, scale = FALSE))
cov(centered.scaled.x <- scale(x)) # all 1
}
\keyword{array}
