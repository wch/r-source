\name{demo}
\alias{demo}
\title{Demonstrations of R Functionality}
\description{
  \code{demo} is a user-friendly interface to running some demonstration
  \R scripts.  \code{demo()} gives the list of available topics.
}
\usage{
demo(topic, package = NULL, lib.loc = NULL,
     character.only = FALSE, verbose = getOption("verbose"))
}
\arguments{
  \item{topic}{the topic which should be demonstrated, given as a
    \link{name} or literal character string, or a character string,
    depending on whether \code{character.only} is \code{FALSE} (default)
    or \code{TRUE}.  If omitted, the list of available topics is
    displayed.}
  \item{package}{a character vector giving the packages to look into for
    demos, or \code{NULL}.  By default, all packages in the search path
    are used.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{character.only}{logical; if \code{TRUE}, use \code{topic} as
    character string.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
}
\details{
  If no topics are given, \code{demo} lists the available demos.  The
  corresponding information is returned in an object of class
  \code{"packageIQR"}.  The structure of this class is experimental.  In
  earlier versions of R, an empty character vector was returned along
  with listing available demos.
}
\seealso{\code{\link{source}} which is called by \code{demo}.}
\examples{
demo() # for attached packages

## All available demos:
demo(package = .packages(all.available = TRUE))

demo(lm.glm, package="stats")
\dontrun{
 ch <- "scoping"
 demo(ch, character = TRUE)
}}
\keyword{documentation}
\keyword{utilities}
