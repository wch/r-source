\name{environment}
\title{Environment Access}
\usage{
environment(fun = NULL)
environment(fun) <- value
is.environment(obj)
.GlobalEnv
}
\alias{environment}
\alias{environment<-}
\alias{.GlobalEnv}
\alias{globalenv}
\alias{is.environment}
\arguments{
\item{fun}{a \code{\link{function}} or \code{NULL}, which is the default.}
}
\value{
If \code{fun} is a function then \code{environment(fun)}
returns the environment associated with that function.
If \code{fun} is \code{NULL} then the global environment, \code{.GlobalEnv},
 is returned. This variable is currently defined as \code{globalenv()}.

The assignment form sets the environment of the function \code{fun}
to the \code{value} given.  \code{is.environment(obj)} returns \code{TRUE}
iff \code{obj} is an \code{environment}.
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

is.environment(.GlobalEnv)# T
}
\keyword{data}
\keyword{programming}
