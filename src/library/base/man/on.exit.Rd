% File src/library/base/man/on.exit.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{on.exit}
\alias{on.exit}
\title{Function Exit Code}
\description{
  \code{on.exit} records the expression given as its argument as needing
  to be executed when the current function exits (either naturally or as
  the result of an error).  This is useful for resetting graphical
  parameters or performing other cleanup actions.

  If no expression is provided, i.e., the call is \code{on.exit()}, then
  the current \code{on.exit} code is removed.
}
\usage{
on.exit(expr = NULL, add = FALSE)
}
\arguments{
  \item{expr}{an expression to be executed.}
  \item{add}{if TRUE, add \code{expr} to be executed after any previously
    set expressions; otherwise (the default) \code{expr} will overwrite
    any previously set expressions.}
}
\details{
  The \code{expr} argument passed to \code{on.exit} is recorded without
  evaluation.  If it is not subsequently removed/replaced by another
  \code{on.exit} call in the same function, it is evaluated in the
  evaluation frame of the function when it exits (including during
  standard error handling).  Thus any functions or variables in the
  expression will be looked for in the function and its environment at
  the time of exit: to capture the current value in \code{expr} use
  \code{\link{substitute}} or similar.

  This is a \sQuote{special} \link{primitive} function: it only
  evaluates the argument \code{add}.
}
\value{
  Invisible \code{NULL}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{sys.on.exit}} which returns the expression stored for use
  by \code{on.exit()} in the function in which \code{sys.on.exit()} is
  evaluated.
}
\examples{
require(graphics)

opar <- par(mai = c(1,1,1,1))
on.exit(par(opar))
\dontshow{par(opar)}
}
\keyword{programming}
