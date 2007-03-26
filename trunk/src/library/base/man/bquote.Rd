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
  \item{expr}{A language object.}
  \item{where}{An environment.}
}

\value{
  A language object.
}
\seealso{
  \code{\link{quote}}, \code{\link{substitute}}
}
\examples{
a <- 2

bquote(a == a)
quote(a == a)

bquote(a == .(a))
substitute(a == A, list(A = a))

plot(1:10, a*(1:10), main = bquote(a == .(a)))
}
\keyword{programming}
\keyword{data}
