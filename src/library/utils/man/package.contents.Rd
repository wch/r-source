\name{package.contents}
\alias{package.contents}
\title{Package Contents}
\description{
  Parses and returns the \file{CONTENTS} file of a package.
}
\usage{
package.contents(pkg, lib.loc = NULL)
}
\arguments{
  \item{pkg}{a character string with the package name.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
}
\value{
  \code{package.contents} returns \code{NA} if there is no
  \file{CONTENTS} file for the given package; otherwise, a character
  matrix with column names \code{c("Entry", "Keywords", "Description")}
  and rows giving the corresponding entries in the CONTENTS data base
  for each Rd file in the package.
}
\seealso{\code{\link{read.dcf}}, \code{\link{package.description}}}
\examples{
package.contents("splines")
package.contents("splines")[, c("Entry", "Description")]
}
\keyword{utilities}
