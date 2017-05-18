% File src/library/base/man/dots.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{dots}
\title{\dots, \code{..1}, etc used in Functions}
\alias{dots}
\alias{...}
\alias{..1}
\alias{..2}
\alias{...elt}
\alias{...length}
\description{
  \code{\dots} and \code{..1}, \code{..2} etc are used to refer to
  arguments passed down from a calling function.  These (and the
  following) can only be used \emph{inside} a function which has
  \code{...} among it formal arguments.

  \code{...elt(n)} is a functional way to get \code{..<n>} and
  basically the same as \code{eval(paste0("..", n))}, just more elegant
  and efficient.
  Note that \code{switch(n, ...)} is very close, differing by returning
  \code{NULL} invisibly instead of an error when \code{n} is zero or
  too large.

  \code{...length()} returns the number of expressions in \code{\dots}.
  This is the same as \code{length(list(...))} but without evaluating
  the expressions in \code{...} (which happens with \code{list(...)}).
}
\usage{
...length()
...elt(n)
}
\arguments{
  \item{n}{a positive integer, not larger than the number of expressions
    in \dots, which is the same as \code{...length()} which is the same
    as \code{length(list(...))}, but the latter evaluates all
    expressions in \code{\dots}.}
}
\seealso{
  \code{\dots} and \code{..1}, \code{..2} are \emph{reserved} words in
  \R, see \code{\link{Reserved}}.

  For more, see the
  \ifelse{html}{\href{/doc/manual/R-intro.html#The-three-dots-argument}{Introduction to R}}{\sQuote{Introduction to R}}
  manual for usage of these syntactic elements,
  and \link[methods]{dotsMethods} for their use in formal (S4) methods.
}
\examples{
tst <- function(n, ...) ...elt(n)
tst(1, pi=pi*0:1, 2:4) ## [1] 0.000000 3.141593
tst(2, pi=pi*0:1, 2:4) ## [1] 2 3 4
try(tst(1)) # -> Error about '...' not containing an element.

tst.dl <- function(x, ...) ...length()
tst.dl(1:10)    # 0  (because the first argument is 'x')
tst.dl(4, 5)    # 1
tst.dl(4, 5, 6) # 2  namely '5, 6'
tst.dl(4, 5, 6, 7, sin(1:10), "foo"/"bar") # 5.  Note: no evaluation!
}
\keyword{programming}
\keyword{documentation}
