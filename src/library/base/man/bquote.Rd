% File src/library/base/man/bquote.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{bquote}
\alias{bquote}
\title{Partial substitution in expressions}
\description{
  An analogue of the LISP backquote macro.  \code{bquote} quotes its
  argument except that terms wrapped in \code{.()} are evaluated in the
  specified \code{where} environment.
}
\usage{
bquote(expr, where = parent.frame())
}
\arguments{
  \item{expr}{A \link{language object}.}
  \item{where}{An environment.}
}

\value{
  A \link{language object}.
}
\seealso{
  \code{\link{quote}}, \code{\link{substitute}}
}
\examples{
require(graphics)

a <- 2

bquote(a == a)
quote(a == a)

bquote(a == .(a))
substitute(a == A, list(A = a))

plot(1:10, a*(1:10), main = bquote(a == .(a)))

## to set a function default arg
default <- 1
bquote( function(x, y = .(default)) x+y )
}
\keyword{programming}
\keyword{data}
