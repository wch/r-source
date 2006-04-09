\name{zpackages}
\title{Listing of Packages}
\alias{.packages}
\description{
  \code{.packages} returns information about package availability.
}
\usage{
.packages(all.available = FALSE, lib.loc = NULL)
}
\arguments{
  \item{all.available}{logical; if \code{TRUE} return a character vector
    of all available packages in \code{lib.loc}.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
}
\details{
  \code{.packages()} returns the \dQuote{base names} of the currently
  attached packages \emph{invisibly} whereas
  \code{.packages(all.available = TRUE)} gives (visibly) \emph{all}
  packages available in the library location path \code{lib.loc}.
  For a package to be regarded as being available it must have a
  \file{DESCRIPTION} file containing a valid \code{version} field.
}
\value{
  A character vector of package \dQuote{base names}, invisible unless
  \code{all.available = TRUE}.
}
\author{
  R core;
  Guido Masarotto for the \code{all.available=TRUE} part of
  \code{.packages}.
}
\seealso{
  \code{\link{library}}, \code{\link{.libPaths}}.
}
\examples{
(.packages())               # maybe just "base"
.packages(all = TRUE)       # return all available as character vector
require(splines)            # the same
(.packages())               # "splines", too
detach("package:splines")
}
\keyword{data}
