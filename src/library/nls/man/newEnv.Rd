\name{newEnv}
\alias{newEnv}
\title{Create an environment}
\description{
    \code{newEnv} returns a new (empty) environment enclosed in the
    \code{enclos} environment.
}
\usage{
newEnv(enclos)
}
\arguments{
    \item{enclos}{An optional enclosing environment for the environment to
	be created.  If \code{enclos} is missing the new environment will
	be enclosed in the parent's environment.}
}
\value{
    A new (empty) environment.
}
\author{Saikat DebRoy <saikat@stat.wisc.edu>}
\section{WARNING }{This function is deprecated and will be removed in
    release 1.2.0.  Use \code{new.env} instead.}
\seealso{\code{\link{new.env}}}
\examples{
myenv <- newEnv()
str(myenv)
}
\keyword{data}
\keyword{programming}
