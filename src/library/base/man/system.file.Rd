\name{system.file}
\title{Find Names of R System Files}
\usage{
system.file(\dots, package = "base", lib.loc = NULL)
}
\alias{system.file}
\arguments{
  \item{\dots}{character strings, specifying subdirectory and file(s)
    within some package.  The default, none, returns the
    root of the package.  Wildcards are not supported.}
  \item{package}{a character string with the name of a single package.
    An error occurs if more than one package name is given.}
  \item{lib.loc}{a character vector with path names of \R libraries, or
    \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
}
\description{
  Finds the full file names of files in packages etc.
}
\value{
  A character vector of positive length, containing the file names
  that matched \code{\dots}, or the empty string, \code{""}, if none
  matched.  If matching the root of a package, there is no trailing
  separator.

  As a special case, \code{system.file()} gives the root of the
  \pkg{base} package only.
}
\seealso{\code{\link{R.home}} for the \dQuote{root} directory of the \R
  installation, \code{\link{list.files}}
}
\examples{
system.file()                  # The root of the 'base' package
system.file(package = "stats") # The root of package 'stats'
system.file("INDEX")
system.file("help", "AnIndex", package = "splines")
}
\keyword{file}
\keyword{utilities}
