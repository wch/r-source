\name{package.contents}
\alias{package.contents}
\alias{package.description}
\title{Package Contents and Description}
\description{
  Parses and returns the \file{CONTENTS} and \file{DESCRIPTION}
  file of a package.
}
\usage{
package.contents(pkg, lib = .lib.loc)
package.description(pkg, lib = .lib.loc, fields = NULL)
}
\arguments{
 \item{pkg}{A character string with the package name.}
 \item{lib}{A character vector with path names of \R package
   libraries.}
 \item{fields}{A character vector of fields to return (if other fields
   occur in the file they are ignored.}
}
\seealso{\code{\link{read.dcf}}}
\examples{
package.contents("mva")
}
\keyword{utilities}
