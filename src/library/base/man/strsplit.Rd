\name{strsplit}
\title{Split the Elements of a Character Vector}
\usage{strsplit(x, split, extended = TRUE)}
\alias{strsplit}
\description{
  Split the elements of a character vector \code{x} into substrings
  according to the presence of substring \code{split} within them.
}
\arguments{
  \item{x}{character vector, to be split.}
  \item{split}{character vector containing a regular expression to use
    as ``split''.  If empty matches occur, in particular if \code{split}
    has length 0, \code{x} is split into single characters.  If
    \code{split} has length greater than 1, it is re-cycled along \code{x}.}
  \item{extended}{if \code{TRUE}, extended regular expression matching
    is used, and if \code{FALSE} basic regular expressions are used.}
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

## a useful function: rev() for strings
strReverse <- function(x)
	sapply(lapply(strsplit(x,NULL), rev), paste, collapse="")
strReverse(c("abc", "Statistics"))

a <- readLines(file.path(R.home(),"AUTHORS"))[-(1:8)]
a <- a[0:1-length(a)]
sub("\t.*","", a)
strReverse(sub(" .*","", a))
}
\keyword{character}
