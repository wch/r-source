\name{QC}
\alias{QC}
\alias{checkAssignFuns}
\alias{checkDocArgs}
\alias{checkDocStyle}
\alias{checkMethods}
\alias{print.checkAssignFuns}
\alias{print.checkDocArgs}
\alias{print.checkDocStyle}
\alias{print.checkMethods}
\title{QC Checks for R Code and/or Documentation}
\description{
  Functions for performing various quality checks.
}
\usage{
checkAssignFuns(package, dir, lib.loc = NULL)
checkDocArgs(package, dir, lib.loc = NULL)
checkDocStyle(package, dir, lib.loc = NULL)
checkMethods(package, dir, lib.loc = NULL)
}
\arguments{
  \item{package}{a character string naming an installed package.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectories \code{R}
    (for R code) and \file{man} with \R documentation sources (in Rd
    format).  Only used if \code{package} is not given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    to search for \code{package}.}
}
\details{
  \code{checkAssignFuns} checks whether assignment functions in the
  package R code have their final argument named \code{value}.
  
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
  method.  As an exception, the first argument of a formula method
  \emph{may} be called \code{formula} even if this is not the name used
  by the generic.  Currently, only functions the body of which starts
  with a call to \code{UseMethod} are recognized as (S3) generics;
  internal method dispatch (see section ``Simple Dispatching'' in
  \link[base]{zMethods}) are not taken into account.  The generics are
  sought first in the given package and then in the base package.  The
  rules when \code{\dots} is involved are subtle: see the source code.

  If using an installed package, the checks needing access to all R
  objects of the package will load the package (unless it is the base
  package), after possibly detaching an already loaded version of the
  package.
}
\value{
  The functions return objects of class the same as the respective
  function names containing the information about problems detected.
  There is a \code{print} method for nicely displaying the information
  contained in such objects.
}
\section{Warning}{
  These functions are still experimental.  Both names, interfaces and
  values might change in future versions.
}
\keyword{utilities}
\keyword{documentation}
