\name{autoload}
\title{On-demand Loading of Packages}
\usage{
autoload(name, package, \dots)
autoloader(name, package, \dots)
.AutoloadEnv
}
\alias{autoload}
\alias{autoloader}
\alias{.AutoloadEnv}
\arguments{
\item{name}{string giving the name of an object.}
\item{package}{string giving the name of a package containing the object.}
\item{\dots}{other arguments to \code{\link{library}}.}
}
\description{
  \code{autoload} creates a promise-to-evaluate \code{autoloader} and stores it with name \code{name} in
  \code{.AutoloadEnv} environment.  When \R attempts to evaluate
\code{name}, \code{autoloader} is run,
  the package is loaded and \code{name} is re-evaluated in the
  new package's environment.  The result is that \R behaves as if
  \code{file} was loaded but it does not occupy memory.
}
\value{
This function is invoked for its side-effect. It has no return value as
of \R 1.7.0.
}
\seealso{
\code{\link{delay}}, \code{\link{library}}
}
\examples{
autoload("line","eda")
search()
ls("Autoloads")

data(cars)
plot(cars)
z<-line(cars)
abline(coef(z))
search()
detach("package:eda")
search()
z<-line(cars)
search()
}
\keyword{data}
\keyword{programming}

