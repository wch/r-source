\name{nchar}
\title{Count the Number of Characters (Bytes)}
\usage{
nchar(x, type = "bytes")
}
\alias{nchar}
\description{
  \code{nchar} takes a character vector as an argument and
  returns a vector whose elements contain the sizes of
  the corresponding elements of \code{x}.
}
\arguments{
  \item{x}{character vector, or a vector to be coerced to a character
    vector.}
  \item{type}{character string: partial matching to one of
    \code{c("bytes", "chars", "width")}.  See Details.}
}
\details{
  The \sQuote{size} of a character string can be measured in one of
  three ways
  \describe{
    \item{\code{bytes}}{The number of bytes needed to store the string
      (plus in C a final terminator which is not counted).}
    \item{\code{chars}}{The number of human-readable characters.}
    \item{\code{width}}{The number of columns \code{cat} will use to
      print the string in a monospaced font.  The same as \code{chars}
      if this cannot be calculated.}
  }
  These will often be the same, and almost always will be in single-byte
  locales.  There will be differences between the first two with
  multibyte character sequences, e.g. in UTF-8 locales.
  If the byte stream contains embedded \code{nul} bytes,
  \code{type = "bytes"} looks at all the bytes whereas the other two
  types look only at the string as printed by \code{cat}, up to the
  first \code{nul} byte.
  
  The internal equivalent of the default method of
  \code{\link{as.character}} is performed on \code{x} (so there is no
  method dispatch).  If you want to operate on non-vector objects
  passing them through \code{\link{deparse}} first will be required.
}
\value{
  An integer vector giving the sizes of each element,
  currently always \code{2} for missing values (for \code{NA}).

  If an element is invalid in a multi-byte character set such as UTF-8,
  its number of characters and the width will be \code{NA}.  Otherwise
  the number of characters will be non-negative, so
  \code{!is.na(nchar(x, "chars"))} is a test of validity.

  Names, dims and dimnames are copied from the input.
}
\note{
  This does \strong{not} by default give the number of characters that
  will be used to \code{print()} the string.  Use
  \code{\link{encodeString}} to find the characters used to print the
  string.

  Embedded \code{nul} bytes are included in the byte count (but not the
  final \code{nul}).
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
