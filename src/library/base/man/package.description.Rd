\name{package.description}
\alias{package.description}
\title{Package Description}
\description{
  Parses and returns the \file{DESCRIPTION} file of a package.
}
\usage{
package.description(pkg, lib.loc = NULL, fields = NULL)
}
\arguments{
  \item{pkg}{a character string with the package name.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{fields}{a character vector giving the tags of fields to return
    (if other fields occur in the file they are ignored).}
}
\value{
  If a \file{DESCRIPTION} for the given package is found and can
  successfully be read, \code{package.description} returns a named
  character vector with the values of the (given) fields as elements and
  the tags as names.  If not, it returns a named vector of \code{NA}s
  with the field tags as names if \code{fields} is not null, and
  \code{NA} otherwise.
}
\seealso{\code{\link{read.dcf}}, \code{\link[utils]{package.contents}}}
\examples{
package.description("ts")
package.description("ts")[c("Package", "Version")]
## NOTE: No subscripting using '$' or abbreviated field tags!
}
\keyword{utilities}
