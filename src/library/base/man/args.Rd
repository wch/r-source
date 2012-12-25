% File src/library/base/man/args.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

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
  For a closure, a closure with identical formal argument list but an
  empty (\code{NULL}) body.

  For a primitive, a closure with the documented usage and \code{NULL}
  body.  Note that some primitives do not make use of named arguments
  and match by position rather than name.

  \code{NULL} in case of a non-function.
}
\details{
  This function is mainly used interactively to print the argument list
  of a function.  For programming, consider using \code{\link{formals}}
  instead.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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
