\name{args}
\alias{args}
\title{Argument List of a Function}
\description{
  Displays the argument names and corresponding default values of a
  function.
}
\usage{
args(name)
}
\arguments{
  \item{name}{an interpreted function.
    If \code{name} is a character string then the function with that
    name is found and used.}
}
\value{
  A function with identical formal argument list but an empty body if
  given an interpreted function; \code{NULL} in case of a variable or
  primitive (non-interpreted) function.
}
\details{
  This function is mainly used interactively.
  For programming, use \code{\link{formals}} instead.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{formals}},
  \code{\link{help}}.
}
\examples{
args(c)            # -> NULL (c is a 'primitive' function)
args(graphics::plot.default)
}
\keyword{documentation}
