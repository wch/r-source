\name{strsplit}
\title{Split the Strings in a Vector}
\usage{strsplit(x, split)}
\alias{strsplit}
\description{
  Split the strings in \code{x} into substrings according to the
  presence of substring \code{split} within them.
}
\arguments{
  \item{x}{character vector, to be split.}
  \item{split}{character string containing a regular expression to use
    as ``split''.  If empty matches occur, in particular if \code{split}
    has length 0, \code{x} is split into single characters.  If
    \code{split} is a vector, it is re-cycled along \code{x}.}
}
\value{
  A list of length \code{length(x)} the \code{i}-th element of which
  contains the vector of splits of \code{x[i]}.
}
\seealso{
  \code{\link{paste}} for the reverse,
  \code{\link{grep}} and \code{\link{sub}} for string search and
  manipulation; further \code{\link{nchar}}, \code{\link{substr}}.
}
\examples{
noquote(strsplit("A text I want to display with spaces", NULL)[[1]])

x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
strsplit(x,"e")

unlist(strsplit("a.b.c", "."))
## [1] "" "" "" "" ""
## Note that `split' is a regexp!
## If you really want to split on `.', use
unlist(strsplit("a.b.c", "\\\\."))
## [1] "a" "b" "c"
}
\keyword{character}
