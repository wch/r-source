\name{environment}
\alias{environment}
\alias{environment<-}
\alias{.GlobalEnv}
\alias{globalenv}
\alias{is.environment}
\alias{new.env}
\title{Environment Access}
\description{
  Get, set, test for and create environments.
}
\usage{
environment(fun = NULL)
environment(fun) <- value
is.environment(obj)
.GlobalEnv
new.env()
}
\arguments{
  \item{fun}{a \code{\link{function}}, a \code{\link{formula}}, or
    \code{NULL}, which is the default.}
  \item{value}{}
  \item{obj}{an arbitrary \R object.}
}
\value{
  If \code{fun} is a function or a formula then \code{environment(fun)}
  returns the environment associated with that function or formula.
  If \code{fun} is \code{NULL} then the global environment,
  \code{.GlobalEnv}, is returned.  This variable is currently defined
  as \code{globalenv()}.

  The assignment form sets the environment of the function or formula
  \code{fun} to the \code{value} given.

  \code{is.environment(obj)} returns \code{TRUE} iff \code{obj} is an
  \code{environment}.

  \code{new.env} returns a new (empty) environment enclosed in the
  parent's environment.
}
\seealso{
  The \code{envir} argument of \code{\link{eval}}.
}
\examples{
##-- all three give the same:
environment()
environment(environment)
.GlobalEnv

ls(envir=environment(approxfun(1:2,1:2, method="const")))

is.environment(.GlobalEnv)# TRUE
}
\keyword{data}
\keyword{programming}
