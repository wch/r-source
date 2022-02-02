\name{chkDots}
\alias{chkDots}
\title{Warn About Extraneous Arguments in the "..." of Its Caller}
\description{
  Warn about extraneous arguments in the \code{...} of its caller.  A
  utility to be used e.g., in S3 methods which need a formal \code{...}
  argument but do not make any use of it.  This helps catching user
  errors in calling the function in question (which is the caller of
  \code{chkDots()}).
}
\usage{
chkDots(..., which.call = -1, allowed = character(0))
}
\arguments{
  \item{...}{\dQuote{the dots}, as passed from the caller.}
  \item{which.call}{passed to \code{\link{sys.call}()}.  A caller may
    use -2 if the message should mention \emph{its} caller.}
  \item{allowed}{not yet implemented: character vector of \emph{named}
    elements in \code{...} which are \dQuote{allowed} and hence not
    warned about.}
}
\author{Martin Maechler, first version outside base, June 2012.}
\seealso{
  \code{\link{warning}},  \code{\link{...}}.
}
\examples{
seq.default ## <- you will see  ' chkDots(...) '

seq(1,5, foo = "bar") # gives warning via chkDots()

## warning with more than one ...-entry:
density.f <- function(x, ...) NextMethod("density")
x <- density(structure(rnorm(10), class="f"), bar=TRUE, baz=TRUE)
}
\keyword{utilities}
