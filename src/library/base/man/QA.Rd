\name{QA}
\alias{checkDocArgs}
\alias{checkDocStyle}
\alias{checkMethods}
\title{QA checks for R code and documentation}
\description{
  Functions for performing various quality checks.
}
\usage{
checkDocArgs(dir)
checkDocStyle(dir)
checkMethods(dir)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectories \code{R}
    (for R code) and \file{man} with \R documentation sources (in Rd
    format).}
}
\details{
  \code{checkDocArgs} checks, for all Rd files in a package, whether all
  arguments shown in the usage sections of the Rd file are documented in
  its arguments section.  It also reports duplicated entries in the
  arguments section.

  \code{checkDocStyle} investigates how (S3) methods are shown in the
  usages of the Rd files in a package.  It reports if methods are shown
  along with their generic, which typically causes problems for the
  documentation of the primary argument of the generic.  It also finds
  the methods shown by their full name (rather than that of the generic
  using the Rd \code{\\method} markup).

  \code{checkMethods} checks whether all methods defined in the package
  R code have all arguments of the corresponding generic, with
  positional arguments of the generics in the same positions for the
  method.  The generics are sought first in the given package and then
  in the base package.  The rules when \code{\dots} is involved are
  subtle: see the source code.
}
\section{Warning}{
  These functions are still experimental.  Both names and interfaces
  might change in future versions.
}
\keyword{utilities}
\keyword{documentation}
