\name{scale}
\title{Scaling and Centering of Matrix-like Objects}
\description{
  \code{scale} is generic function who's default method
  centers and/or scales the columns of a numeric matrix.
}
\alias{scale}
\alias{scale.default}
\usage{
scale(x, center = TRUE, scale = TRUE)
}
\synopsis{
scale.default(x, center = TRUE, scale = TRUE)
scale(x, ..., scale = TRUE)
}
\arguments{
  \item{x}{a numeric matrix(like object).}
  \item{center}{either a logical value or a numeric vector of length
    equal to the number of columns of \code{x}.}
  \item{scale}{either a logical value or a numeric vector of length
    equal to the number of columns of \code{x}.}
}
\value{
  For \code{scale.default}, the centered, scaled matrix.
}
\details{
  The value of \code{center} determines how column centering is
  performed.
  If \code{center} is a numeric vector with length equal to the number
  of columns of \code{x}, then each column of \code{x} has the
  corresponding value from \code{center} subtracted from it.
  If \code{center} is \code{TRUE} then centering is done by subtracting
  the column means of \code{x} from their corresponding columns, and if
  \code{center} is \code{FALSE}, no centering is done.

  The value of \code{scale} determines how column scaling is performed
  (after centering).
  If \code{scale} is a numeric vector with length equal to the number of
  columns of \code{x}, then each column of \code{x} is divided by the
  corresponding value from \code{scale}.
  If \code{scale} is \code{TRUE} then scaling is done by dividing the
  (centered) columns of \code{x} by their root-mean-square, and if
  \code{scale} is \code{FALSE}, no scaling is done.

  The root-mean-square for a column is obtained by computing the
  square-root of the sum-of-squares of the non-missing values in the
  column divided by the number of non-missing values minus one.
}
\seealso{
  \code{\link{sweep}} which allows centering (and scaling) with
  arbitrary statistics.
}
\examples{
x <- matrix(1:10, nc=2)
(centered.x <- scale(x, scale=FALSE))
cov(centered.scaled.x <- scale(x))# all 1
}
\keyword{array}
