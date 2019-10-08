% File src/library/base/man/bquote.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{bquote}
\alias{bquote}
\title{Partial substitution in expressions}
\description{
  An analogue of the LISP backquote macro.  \code{bquote} quotes its
  argument except that terms wrapped in \code{.()} are evaluated in the
  specified \code{where} environment. If \code{splice = TRUE} then
  terms wrapped in \code{..()} are evaluated and spliced into a call.
}
\usage{
bquote(expr, where = parent.frame(), splice = FALSE)
}
\arguments{
  \item{expr}{A \link{language object}.}
  \item{where}{An environment.}
  \item{splice}{Logical; if \code{TRUE} splicing is enabled.}
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

exprs <- expression(x <- 1, y <- 2, x + y)
bquote(function() {..(exprs)}, splice = TRUE)
}

\keyword{programming}
\keyword{data}
