% File src/library/tools/man/makeLazyLoading.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{makeLazyLoading}
\alias{makeLazyLoading}
\title{Lazy Loading of Packages}
\usage{
makeLazyLoading(package, lib.loc = NULL, compress = TRUE,
                keep.source = getOption("keep.source.pkgs"),
                keep.parse.data = getOption("keep.parse.data.pkgs"))
}
\arguments{
  \item{package}{package name string}
  \item{lib.loc}{library trees, as in \code{library}}
  \item{keep.source}{logical; should sources be kept when saving from source}
  \item{keep.parse.data}{logical; should parse data be kept with sources
    when \code{keep.source} is \code{TRUE}.}
  \item{compress}{logical; whether to compress entries on the database.}
}
\description{
  Tools for lazy loading of packages from a database.
}
\details{
  A tool to set up packages for lazy loading from a database.  For
  packages other than \pkg{base} you can use
  \code{makeLazyLoading(package)} to convert them to use lazy loading.

  This is done when a package is installed.
}
\keyword{utilities}
\author{Luke Tierney and Brian Ripley}
\keyword{internal}

