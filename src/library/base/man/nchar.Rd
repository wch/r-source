\name{nchar}
\title{Count the Number of Characters (Bytes)}
\usage{nchar(x)}
\alias{nchar}
\description{
  \code{nchar} takes a character vector as an argument and
  returns a vector whose elements contain the number of bytes in
  the corresponding element of \code{x}.
}
\arguments{
  \item{x}{character vector, or a vector to be coerced to a character
    vector.} 
}
\details{
  The internal equivalent of the default method of
  \code{\link{as.character}} is performed on \code{x}.  If you want to
  operate on non-vector objects passing them through
  \code{\link{deparse}} first will be required.
}
\value{
  An integer vector giving the number of bytes in each string,
  \code{2} for missing values (for \code{NA}).
}
\note{
  This does \strong{not} give the number of characters that will be used
  to print the string, although it was documented to do so up to \R
  2.0.1.  Use \code{\link{encodeString}} to find the characters used to
  print the string.

  It is possible that multibyte sequences might be displayed as a single
  character.

  As from \R 2.1.0 embedded \code{nul} bytes are included in the
  count (but not the final \code{nul}): previously the count stopped
  immediately before the first \code{nul}.
}
\section{Warning}{
  Whether this should return the number of \emph{bytes} or the number of
  \emph{characters} in a multibyte encoding (e.g. UTF-8) is currently
  undecided.
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
