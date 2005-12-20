\name{package.skeleton}
\alias{package.skeleton}
\title{Create a Skeleton for a New Source Package}
\description{
  \code{package.skeleton} automates some of the setup for a new source
  package.  It creates directories, saves functions and data to
  appropriate places, and creates skeleton help files and \file{README}
  files describing further steps in packaging.
}
\usage{
package.skeleton(name = "anRpackage", list,
                 environment = .GlobalEnv,
	         path = ".", force = FALSE)
}
\arguments{
  \item{name}{character string: the directory name for your package.}
  \item{list}{character vector naming the \R objects to put in the package.}
  \item{environment}{if \code{list} is omitted, the contents of this
    environment are packaged.}
  \item{path}{path to put the package directory in.}
  \item{force}{If \code{FALSE} will not overwrite an existing directory.}
}
\value{
  used for its side-effects.
}
\details{
  The package sources are placed in subdirectory \code{name} of \code{path}.
  
  This tries to create filenames valid for all OSes known to run R.
  Invalid characters are replaced by \code{_}, invalid names are
  preceded by \code{zz}, and finally the converted names are made
  unique by \code{\link{make.unique}(sep = "_")}.  This can be done for
  code and help files but not data files (which are looked for by name).
}

\references{
  Read the \emph{Writing R Extensions} manual for more details.

  Once you have created a \emph{source} package you need to install it:
  see the \emph{R Installation and Administration} manual,
  \code{\link{INSTALL}} and \code{\link{install.packages}}.
}
\seealso{
  \code{\link{prompt}}
}

\examples{
## two functions and two "data sets" :
f <- function(x,y) x+y
g <- function(x,y) x-y
d <- data.frame(a=1, b=2)
e <- rnorm(1000)
\dontshow{
  owd <- getwd()
  setwd(tempdir())
}
package.skeleton(list=c("f","g","d","e"), name="mypkg")
\dontshow{ setwd(owd) }
}
\keyword{file}
\keyword{utilities}
