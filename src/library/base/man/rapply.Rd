% File src/library/base/man/rapply.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{rapply}
\alias{rapply}
\title{Recursively Apply a Function to a List}
\description{
  \code{rapply} is a recursive version of \code{\link{lapply}}.
}
\usage{
rapply(object, f, classes = "ANY", deflt = NULL,
       how = c("unlist", "replace", "list"), ...)
}
\arguments{
  \item{object}{A list.}
  \item{f}{A function of a single argument.}
  \item{classes}{A character vector of \code{\link{class}} names, or
    \code{"ANY"} to match any class.}
  \item{deflt}{The default result (not used if \code{how = "replace"}).}
  \item{how}{A character string partially matching the three possibilities given:
    see \sQuote{Details}.}
  \item{\dots}{additional arguments passed to the call to \code{f}.}
}
\details{
  This function has two basic modes.  If \code{how = "replace"}, each
  element of the list which is not itself a list and has a class
  included in \code{classes} is replaced by the result of applying
  \code{f} to the element.

  If the mode is \code{how = "list"} or \code{how = "unlist"}, the list
  is copied, all non-list elements which have a class included in
  \code{classes} are replaced by the result of applying \code{f} to the
  element and all others are replaced by \code{deflt}.  Finally, if
  \code{how = "unlist"}, \code{unlist(recursive = TRUE)} is called on
  the result.

  The semantics differ in detail from \code{\link{lapply}}: in
  particular the arguments are evaluated before calling the C code.
}
\value{
  If \code{how = "unlist"}, a vector, otherwise a list of similar
  structure to \code{object}.
}
\seealso{
  \code{\link{lapply}}, \code{\link{dendrapply}}.
}
\references{
  Chambers, J. A. (1998)
  \emph{Programming with Data}.
  Springer.\cr
  (\code{rapply} is only described briefly there.)
}
\examples{
X <- list(list(a = pi, b = list(c = 1:1)), d = "a test")
rapply(X, function(x) x, how = "replace")
rapply(X, sqrt, classes = "numeric", how = "replace")
rapply(X, nchar, classes = "character",
       deflt = as.integer(NA), how = "list")
rapply(X, nchar, classes = "character",
       deflt = as.integer(NA), how = "unlist")
rapply(X, nchar, classes = "character", how = "unlist")
rapply(X, log, classes = "numeric", how = "replace", base = 2)
}
\keyword{iteration}
\keyword{list}
