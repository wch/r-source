\name{package.skeleton}
\alias{package.skeleton}
\title{Create a skeleton for a new package}
\description{
  \code{package.skeleton} automates some of the setup for a new
  package.  It creates directories, saves functions and data to
  appropriate places, and creates skeleton help files and \file{README}
  files describing further steps in packaging.
}
\usage{
package.skeleton(name="anRpackage", list, environment=.GlobalEnv, 
	path=".", force=FALSE)
}
\arguments{
  \item{name}{directory name for your package}
  \item{list}{vector of names of R objects to put in the package}
  \item{environment}{if \code{list} is omitted, the contents of this
    environment are packaged}
  \item{path}{path to put the package directories in}
  \item{force}{If \code{FALSE} will not overwrite an existing directory}
}
\value{
  used for its side-effects.
}

\references{Read the \emph{Writing R Extensions} manual for more details }
\seealso{\code{\link{install.packages}}}

\examples{
\dontrun{
    f<-function(x,y) x+y
    g<-function(x,y) x-y
    d<-data.frame(a=1,b=2)
    e<-rnorm(1000)
    package.skeleton(list=c("f","g","d","e"),name="AnExample")
}
}

\keyword{file}
\keyword{utilities}
