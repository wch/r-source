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
globalenv()
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
  If \code{fun} is \code{NULL} then the current evaluation environment is
  returned.

  The assignment form sets the environment of the function or formula
  \code{fun} to the \code{value} given.

  \code{is.environment(obj)} returns \code{TRUE} iff \code{obj} is an
  \code{environment}.

  \code{new.env} returns a new (empty) environment enclosed in the
  parent's environment.
}
\details{
  The global environment \code{.GlobalEnv} is the first item on the
  search path, more often known as the user's workspace.  It can also be
  accessed by \code{globalenv()}.
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
