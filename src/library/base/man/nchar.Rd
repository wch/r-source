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
  An integer vector giving the number of characters in each string
  (up to the first \code{nul} character).  Missing values are regarded
  as having two characters, for \code{"NA"}.
}
\note{
  This does \strong{not} give the number of characters that will be used
  to print the string, although it was documented to do so up to \R
  2.0.1.  Because of escape sequences for non-printable characters, the
  latter may be greater.  Also, it is possible (but unsupported) that
  multichar sequences might be displayed as a single character.
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
# 18 17
}
\keyword{character}
