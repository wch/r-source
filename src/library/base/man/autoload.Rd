\name{autoload}
\title{On-demand loading of packages}
\usage{
autoload(name, file)
autoloader(name, file)
.AutoloadEnv
}
\alias{autoload}
\alias{autoloader}
\alias{.AutoloadEnv}
\arguments{
\item{name}{string giving the name of an object}
\item{file}{string giving the name of a package containing the object}
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
see description
}
\seealso{
\code{\link{delay}}, \code{\link{library}}
}
\examples{
autoload("line","eda")
search()
ls("Autoloads")
all(ls("Autoloads") == ls(envir = .AutoloadEnv))
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

