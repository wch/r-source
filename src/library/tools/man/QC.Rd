% File src/library/tools/man/QC.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{QC}
\title{QC Checks for R Code and/or Documentation}
\alias{QC}
\alias{checkDocFiles}
\alias{checkDocStyle}
\alias{checkReplaceFuns}
\alias{checkS3methods}
\alias{nonS3methods}
\alias{langElts}
\alias{print.checkDocFiles}
\alias{print.checkDocStyle}
\alias{print.checkReplaceFuns}
\alias{print.checkS3methods}
\description{
  Functions for performing various quality control (QC) checks on R code
  and documentation, notably on R packages.
}
\usage{
checkDocFiles(package, dir, lib.loc = NULL)
checkDocStyle(package, dir, lib.loc = NULL)
checkReplaceFuns(package, dir, lib.loc = NULL)
checkS3methods(package, dir, lib.loc = NULL)

langElts
nonS3methods(package)
}
\arguments{
  \item{package}{a character string naming an installed package.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectories \file{R}
    (for R code) and \file{man} with \R documentation sources (in Rd
    format).  Only used if \code{package} is not given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
}
\details{
  \code{checkDocFiles} checks, for all Rd files in a package, whether all
  arguments shown in the usage sections of the Rd file are documented in
  its arguments section.  It also reports duplicated entries in the
  arguments section, and \sQuote{over-documented} arguments which are
  given in the arguments section but not in the usage.  Note that the
  match is for the usage section and not a possibly existing synopsis
  section, as the usage is what gets displayed.

  \code{checkDocStyle} investigates how (S3) methods are shown in the
  usages of the Rd files in a package.  It reports the methods shown by
  their full name rather than using the Rd \verb{\method} markup for
  indicating S3 methods.  Earlier versions of \R also reported about
  methods shown along with their generic, which typically caused
  problems for the documentation of the primary argument in the generic
  and its methods.  With \verb{\method} now being expanded in a way
  that class information is preserved, joint documentation is
  no longer necessarily a problem.  (The corresponding information is
  still contained in the object returned by \code{checkDocStyle}.)

  \code{checkReplaceFuns} checks whether replacement functions or S3/S4
  replacement methods in the package R code have their final argument
  named \code{value}.

  \code{checkS3methods} checks whether all S3 methods defined in the
  package R code have all arguments of the corresponding generic, with
  positional arguments of the generics in the same positions for the
  method.  As an exception, the first argument of a formula method
  \emph{may} be called \code{formula} even if this is not the name used
  by the generic.  The rules when \code{\dots} is involved are subtle:
  see the source code.  Functions recognized as S3 generics are those
  with a call to \code{UseMethod} in their body, internal S3 generics
  (see \link{InternalMethods}), and S3 group generics (see
  \code{\link[=S3groupGeneric]{Math}}).  Possible dispatch under a
  different name is not taken into account.  The generics are sought
  first in the given package, then in the \pkg{base} package and
  (currently) the packages \pkg{graphics}, \pkg{stats}, and \pkg{utils}
  added in R 1.9.0 by splitting the former \pkg{base}, and, if an
  installed package is tested, also in the loaded namespaces/packages
  listed in the package's \file{DESCRIPTION} Depends field.

  \code{nonS3methods(package)} returns a \code{\link{character}} vector with
  the names of the functions in \code{package} which \sQuote{look} like
  S3 methods, but are not.  Using \code{package = NULL} returns all
  known examples.

  \code{langElts} is a character vector of names of \dQuote{language
    elements} of \R.  These are implemented as \dQuote{very primitive}
  functions (no argument list; \code{\link{print}()}ing as
  \code{.Primitive("<name>")}).

  If using an installed package, the checks needing access to all \R
  objects of the package will load the package (unless it is the
  \pkg{base} package), after possibly detaching an already loaded
  version of the package.
}
\value{
  The functions return objects of class the same as the respective
  function names containing the information about problems detected.
  There are \code{print} methods for nicely displaying the information
  contained in such objects.
}
\keyword{utilities}
\keyword{documentation}
