\name{package.contents}
\alias{package.contents}
\alias{package.description}
\title{Package Contents and Description}
\description{
  Parses and returns the \file{CONTENTS} and \file{DESCRIPTION} file of
  a package.
}
\usage{
package.contents(pkg, lib = .lib.loc)
package.description(pkg, lib = .lib.loc, fields = NULL)
}
\arguments{
  \item{pkg}{a character string with the package name.}
  \item{lib}{a character vector with path names of \R package
    libraries.}
  \item{fields}{a character vector giving the tags of fields to return
    (if other fields occur in the file they are ignored).}
}
\value{
  \code{package.contents} returns \code{NA} if there is no
  \file{CONTENTS} file for the given package; otherwise, a character
  matrix with column names \code{c("Entry", "Keywords", "Description")}
  and rows giving the corresponding entries in the CONTENTS data base
  for each Rd file in the package.

  If a \file{DESCRIPTION} for the given package is found and can
  successfully be read, \code{package.description} returns a named
  character vector with the values of the (given) fields as elements and
  the tags as names.  If not, it returns a named vector of \code{NA}s
  with the field tags as names if \code{fields} is not null, and
  \code{NA} otherwise.
}
\seealso{\code{\link{read.dcf}}}
\examples{
package.contents("mva")
package.contents("mva")[, c("Entry", "Description")]

package.description("ts")
package.description("ts")[c("Package", "Version")]
## NOTE: No subscripting using `$' or abbreviated field tags!
}
\keyword{utilities}
