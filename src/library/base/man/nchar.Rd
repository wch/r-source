\name{nchar}
\title{Count the Number of Characters}
\usage{nchar(x)}
\alias{nchar}
\description{
  \code{nchar} takes a character vector as an argument and
  returns a vector whose elements contain the number of characters in
  the corresponding element of \code{x}.
}
\arguments{
  \item{x}{character vector, or a vector to be coerced to a character
    vector.} 
}
\details{
  The internal equivalent of \code{\link{as.character}} is performed on
  \code{x}. If you want to operate
  on non-vector objects passing them through \code{\link{deparse}} first
  will be required.
}
\value{
  The number of characters as the string will be printed (integer
  \code{2} for a missing string).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{strwidth}} giving width of strings for plotting;
  \code{\link{paste}}, \code{\link{substr}}, \code{\link{strsplit}}
}
\examples{
x <- c("asfef","qwerty","yuiop[","b","stuff.blah.yech")
nchar(x)
# 5  6  6  1 15

nchar(deparse(mean))
# 23  1 16 45 11 64  2 17 50 43  2 17  1
}
\keyword{character}
