\name{makeLazyLoading}
\alias{makeLazyLoading}
\title{Lazy Loading of Packages}
\usage{
makeLazyLoading(package, lib.loc = NULL, compress = TRUE,
                keep.source = getOption("keep.source.pkgs"))
}
\arguments{
  \item{package}{package name string}
  \item{lib.loc}{library trees, as in \code{library}}
  \item{keep.source}{logical; should sources be kept when saving from source}
  \item{compress}{logical; whether to compress entries on the database.}
}
\description{
  Tools for Lazy Loading of Packages from a Database.
}
\details{
  A tool to set up packages for lazy loading from a database.  For
  packages other than base you can use \code{makeLazyLoading(package)}
  to convert them to use lazy loading.
}
\examples{
  # set up package "splines" for lazy loading -- already done
  \dontrun{
    tools:::makeLazyLoading("splines")
  }
}
\keyword{utilities}
\author{Luke Tierney and Brian Ripley}
