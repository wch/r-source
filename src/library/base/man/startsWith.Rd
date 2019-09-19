\name{startsWith}
\title{Does String Start or End With Another String?}
\alias{endsWith}
\alias{startsWith}
\description{
  Determines if entries of \code{x} start or end with string (entries of)
  \code{prefix} or \code{suffix} respectively, where strings are
  recycled to common lengths.
}
\usage{
startsWith(x, prefix)
  endsWith(x, suffix)
}
\arguments{
  \item{x}{vector of \code{\link{character}} string whose \dQuote{starts}
    are considered.}
  \item{prefix, suffix}{\code{\link{character}} vector (often of length one).}
}
\details{
  \code{startsWith()} is equivalent to but much faster than
\preformatted{  substring(x, 1, nchar(prefix)) == prefix  }
  or also
\preformatted{  grepl("^<prefix>", x)  }
  where \code{prefix} is not to contain special regular expression
  characters (and for \code{grepl}, \code{x} does not contain missing
  values, see below).
 
  The code has an optimized branch for the most common usage in which
  \code{prefix} or \code{suffix} is of length one, and is further
  optimized in a UTF-8 or 8-byte locale if that is an ASCII string.
}
\value{
  A \code{\link{logical}} vector, of \dQuote{common length} of \code{x}
  and \code{prefix} (or \code{suffix}), i.e., of the longer of the two
  lengths unless one of them is zero when the result is
  also of zero length.  A shorter input is recycled to the output length.
}
%\author{Martin Maechler}
\seealso{
  \code{\link{grepl}}, \code{\link{substring}}; the partial string
  matching functions \code{\link{charmatch}} and \code{\link{pmatch}}
  solve a different task.
}
\encoding{UTF-8}
\examples{
startsWith(search(), "package:") # typically at least two FALSE, nowadays often three

x1 <- c("Foobar", "bla bla", "something", "another", "blu", "brown",
        "blau blüht der Enzian")# non-ASCII
x2 <- cbind(
      startsWith(x1, "b"),
      startsWith(x1, "bl"),
      startsWith(x1, "bla"),
        endsWith(x1, "n"),
        endsWith(x1, "an"))
rownames(x2) <- x1; colnames(x2) <- c("b", "b1", "bla", "n", "an")
x2

## Non-equivalence in case of missing values in 'x', see Details:
x <- c("all", "but", NA_character_)
cbind(startsWith(x, "a"),
      substring(x, 1L, 1L) == "a",
      grepl("^a", x))
}
\keyword{character}
\keyword{utilities}
