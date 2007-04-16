\name{args}
\alias{args}
\title{Argument List of a Function}
\description{
  Displays the argument names and corresponding default values of a
  function or primitive.
}
\usage{
args(name)
}
\arguments{
  \item{name}{a function (a closure or a primitive).
    If \code{name} is a character string then the function with that
    name is found and used.}
}
\value{
  A function with identical formal argument list but an empty body if
  given a function; \code{NULL} in case of a variable.
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
args(c)
args(graphics::plot.default)
}
\keyword{documentation}
