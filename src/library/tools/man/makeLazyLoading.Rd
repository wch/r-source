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
  Tools for lazy loading of packages from a database.
}
\details{
  This package provides tools to set up packages for lazy loading from a
  database.  For packages other than base you can use
  \code{makeLazyLoading(package)} to convert them to use lazy loading.

  For base you need to start R with no packages loaded,  e.g. by setting
  \code{R_DEFAULT_PACKAGES} to \code{NULL}, and then source
  \code{makebasedb.R}; this file is provided in the package diractory.
  This builds a database.  Once the database has been built, use
  makeLazyLoading("base") to convert base to use lazy loading.

  A simple \code{make} seems sufficient to undo this for base and base
  packages other than \code{methods} and perhaps \code{mle}.  For
  \code{methods} (and presumably others created with \code{--save}) it
  appears that you need to remove \code{methods/R/methods} and then use
  \code{make}.  You may also need more radical action for recommended
  packages.
}
\examples{
  # set up package "base" for lazy loading (must have no packages loaded)
  \dontrun{
    source(file.path(.find.package("tools"),"makebasedb.R"))
    tools:::makeLazyLoading("base")
  }

  # set up package "splines" for lazy loading
  \dontrun{
    tools:::makeLazyLoading("splines")
  }

  # set up all base packages for lazy loading
  \dontrun{
    for (p in rev(installed.packages(priority="base")[,"Package"])) {
        cat(paste("converting", p, "..."))
        tools:::makeLazyLoading(p)
        cat("done\n")
    }
  }
}
\keyword{utilities}
\author{Luke Tierney}
