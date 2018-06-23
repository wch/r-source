% File src/library/base/man/rapply.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{rapply}
\alias{rapply}
\title{Recursively Apply a Function to a List}
\description{
  \code{rapply} is a recursive version of \code{\link{lapply}} with
  flexibility in \emph{how} the result is structured (\code{how = ".."}).
}
\usage{
rapply(object, f, classes = "ANY", deflt = NULL,
       how = c("unlist", "replace", "list"), ...)
}
\arguments{
  \item{object}{a \code{\link{list}} or \code{\link{expression}}, i.e., \dQuote{list-like}.}
  \item{f}{a \code{\link{function}} of one \dQuote{principal} argument,
    passing further arguments via \code{\dots}.}
  \item{classes}{character vector of \code{\link{class}} names, or
    \code{"ANY"} to match any class.}
  \item{deflt}{The default result (not used if \code{how = "replace"}).}
  \item{how}{character string partially matching the three possibilities given:
    see \sQuote{Details}.}
  \item{\dots}{additional arguments passed to the call to \code{f}.}
}
\details{
  This function has two basic modes.  If \code{how = "replace"}, each
  element of \code{object} which is not itself list-like and has a class
  included in \code{classes} is replaced by the result of applying
  \code{f} to the element.

  Otherwise, with mode \code{how = "list"} or \code{how = "unlist"},
  conceptually \code{object}
  is copied, all non-list elements which have a class included in
  \code{classes} are replaced by the result of applying \code{f} to the
  element and all others are replaced by \code{deflt}.  Finally, if
  \code{how = "unlist"}, \code{unlist(recursive = TRUE)} is called on
  the result.

  The semantics differ in detail from \code{\link{lapply}}: in
  particular the arguments are evaluated before calling the C code.

  In \R 3.5.x and earlier, \code{object} was required to be a list,
  which was \emph{not} the case for its list-like components.
}
\value{
  If \code{how = "unlist"}, a vector, otherwise \dQuote{list-like}
  of similar structure as \code{object}.
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
X <- list(list(a = pi, b = list(c = 1L)), d = "a test")
# the "identity operation":
rapply(X, function(x) x, how = "replace") -> X.; stopifnot(identical(X, X.))
rapply(X, sqrt, classes = "numeric", how = "replace")
rapply(X, deparse, control = "all") # passing extras. argument of deparse()
rapply(X, nchar, classes = "character", deflt = NA_integer_, how = "list")
rapply(X, nchar, classes = "character", deflt = NA_integer_, how = "unlist")
rapply(X, nchar, classes = "character",                      how = "unlist")
rapply(X, log, classes = "numeric", how = "replace", base = 2)

## with expression() / list():
E  <- expression(list(a = pi, b = expression(c = C1 * C2)), d = "a test")
LE <- list(expression(a = pi, b = expression(c = C1 * C2)), d = "a test")
rapply(E, nchar, how="replace") # "expression(c = C1 * C2)" are 23 chars
rapply(E, nchar, classes = "character", deflt = NA_integer_, how = "unlist")
rapply(LE, as.character) # a "pi" | b1 "expression" | b2 "C1 * C2" ..
rapply(LE, nchar)        # (see above)
stopifnot(exprs = {
  identical(E , rapply(E , identity, how = "replace"))
  identical(LE, rapply(LE, identity, how = "replace"))
})
}
\keyword{iteration}
\keyword{list}
