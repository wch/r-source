\name{strsplit}
\alias{strsplit}
\title{Split the Elements of a Character Vector}
\description{
  Split the elements of a character vector \code{x} into substrings
  according to the presence of substring \code{split} within them.
}
\usage{
strsplit(x, split, extended = TRUE, fixed = FALSE, perl = FALSE)
}
\arguments{
  \item{x}{
    character vector, each element of which is to be split.
  }
  \item{split}{
    character vector containing \link{regular expression}(s)
    (unless \code{fixed = TRUE}) to use as \dQuote{split}.  If empty
    matches occur, in particular if \code{split} has length 0, \code{x} is
    split into single characters.  If \code{split} has length greater than
    1, it is re-cycled along \code{x}.
  }
  \item{extended}{
    logical. if \code{TRUE}, extended regular expression matching
    is used, and if \code{FALSE} basic regular expressions are used.
  }
  \item{fixed}{
    logical. If \code{TRUE} match string exactly, otherwise
    use regular expressions.
  }
  \item{perl}{
    logical. Should perl-compatible regexps be used?
    Has priority over \code{extended}.
  }
}
\value{
  A list of length \code{length(x)} the \code{i}-th element of which
  contains the vector of splits of \code{x[i]}.
}
\details{
  Arguments \code{x} and \code{split} will be coerced to character, so
  you will see uses with \code{split = NULL} to mean
  \code{split = character(0)}, including in the examples below.

  Note that spltting into single characters can be done via
  \code{split=character(0)} or \code{split=""}; the two are equivalent
  as from \R 1.9.0.

  A missing value of \code{split} does not split the the corresponding
  element(s) of \code{x} at all.
}
\section{Warning}{
  The standard regular expression code has been reported to be very slow
  when applied to extremely long character strings
  (tens of thousands of characters or more): the code used when
  \code{perl=TRUE} seems much faster and more reliable for such usages.

  The \code{perl = TRUE} option is only implemented for singlebyte and
  UTF-8 encodings, and will warn if used in a non-UTF-8 multibyte locale.
}
\seealso{
  \code{\link{paste}} for the reverse,
  \code{\link{grep}} and \code{\link{sub}} for string search and
  manipulation; further \code{\link{nchar}}, \code{\link{substr}}.

  \link{regular expression} for the details of the pattern specification.
}
\examples{
noquote(strsplit("A text I want to display with spaces", NULL)[[1]])

x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
strsplit(x,"e")

unlist(strsplit("a.b.c", "."))
## [1] "" "" "" "" ""
## Note that 'split' is a regexp!
## If you really want to split on '.', use
unlist(strsplit("a.b.c", "\\\\."))
## [1] "a" "b" "c"
## or
unlist(strsplit("a.b.c", ".", fixed = TRUE))

## a useful function: rev() for strings
strReverse <- function(x)
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
strReverse(c("abc", "Statistics"))

## get the first names of the members of R-core
a <- readLines(file.path(R.home(),"AUTHORS"))[-(1:8)]
a <- a[(0:2)-length(a)]
(a <- sub(" .*","", a))
# and reverse them
strReverse(a)
}
\keyword{character}
