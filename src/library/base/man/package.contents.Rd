\name{package.contents}
\alias{package.contents}
\alias{package.description}
\title{Package Contents and Description}
\description{
  Parses and returns the CONTENTS and DESCRIPTION file of a package.
}
\usage{
package.contents(pkg, lib=.lib.loc)
package.description(pkg, lib=.lib.loc)
}
\arguments{
 \item{pkg}{A character string with the package name.}
 \item{lib}{A character vector with path names of R package
             libraries.}
}
\seealso{\code{\link{parse.dcf}}}
\examples{
package.contents("mva")
}
\keyword{utilities}
