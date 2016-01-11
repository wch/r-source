\name{startsWith}
\title{Does String Start or End With Another String?}
\alias{endsWith}
\alias{startsWith}
\description{
  Determines if entries of \code{x} start or end with string (entries of)
  \code{prefix} or \code{suffix} respectively, where strings are
  recycled to common lengths.

  \code{startsWith()} is equivalent to but faster than
  \preformatted{substring(x, 1, nchar(prefix)) == prefix}
  or also (much faster than) \preformatted{grepl("^<prefix>", x)}
  where \code{prefix} is not to contain special regular expression
  characters.
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
%% \details{
%% }
\value{
  a \code{\link{logical}} vector, of \dQuote{common length} of \code{x}
  and \code{prefix} (or \code{suffix}), i.e., of the longer of the two
  \code{\link{length}}s unless one of them is zero when the result is
  also of zero length.
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
cbind(x1,
      startsWith(x1, "b"),
      startsWith(x1, "bl"),
      startsWith(x1, "bla"),
        endsWith(x1, "n"),
        endsWith(x1, "an"))
}
\keyword{character}
\keyword{utilities}
