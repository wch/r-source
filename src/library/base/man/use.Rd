% File src/library/base/man/use.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2023 R Core Team
% Distributed under GPL 2 or later

\name{use}
\title{Use Packages}
\alias{use}
\description{
  Use packages in R scripts by loading their namespace and attaching a
  package environment including (a subset of) their exports to the
  search path.
}
\usage{
use(package, include.only)
}
\arguments{
  \item{package}{a character string given the name of a package.}
  \item{include.only}{character vector of names of objects to
    include in the attached environment frame.  If missing, all exports
    are included.}
}
\details{
  This is a simple wrapper around \code{\link{library}} which always
  uses \code{attach.required = FALSE}, so that packages listed in the
  \code{Depends} clause of the \code{DESCRIPTION} file of the package to
  be used never get attached automatically to the search path.

  This therefore allows to write R scripts with full control over what
  gets found on the search path.  In addition, such scripts can easily
  be integrated as package code, replacing the calls to \code{use} by
  the corresponding \code{ImportFrom} directives in \file{NAMESPACE}
  files.
}
\value{
  (invisibly) a logical indicating whether the package to be used is
  available.
}
\note{
  This functionality is still experimental: interfaces may change in
  future versions.
}
