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
package.skeleton(name = "anRpackage", list, environment = .GlobalEnv, 
	         path = ".", force = FALSE)
}
\arguments{
  \item{name}{character string: the directory name for your package.}
  \item{list}{character vector naming the R objects to put in the package}
  \item{environment}{if \code{list} is omitted, the contents of this
    environment are packaged.}
  \item{path}{path to put the package directories in.}
  \item{force}{If \code{FALSE} will not overwrite an existing directory.}
}
\value{
  used for its side-effects.
}
\details{
  This tries to create filenames valid for all OSes known to run R.
  Invalid characters are replaced by \code{_}, invalid names are
  preceeded by \code{zz}, and finally the converted names are made
  unique by \code{\link{make.unique}(sep = "_")}.  This can be done for
  code and help files but not data files (which are looked for by name).
  
  By default, the new package is created as a source package
  in a subdirectory of the current directory.  It will normally need to
  be installed (using \code{R CMD INSTALL ...}) before being
  available for use.
#ifdef windows
  Windows users will need to install various development tools to do this.
  Instructions are given in \file{readme.packages}.
#endif
}

\references{Read the \emph{Writing R Extensions} manual for more details }
\seealso{
  \code{\link{prompt}}, \code{\link{install.packages}}
}

\examples{
\dontrun{
    f <- function(x,y) x+y
    g <- function(x,y) x-y
    d <- data.frame(a=1,b=2)
    e <- rnorm(1000)
#ifdef windows    
    setwd(Sys.getenv("R_USER"))
#endif
    package.skeleton(list=c("f","g","d","e"), name="AnExample")
}
}

\keyword{file}
\keyword{utilities}
