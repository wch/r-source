% File src/library/tools/man/codoc.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{codoc}
\alias{codoc}
\alias{codocClasses}
\alias{codocData}
\alias{print.codoc}
\alias{print.codocClasses}
\alias{print.codocData}
\title{Check Code/Documentation Consistency}
\usage{
codoc(package, dir, lib.loc = NULL,
      use.values = NULL, verbose = getOption("verbose"))
codocClasses(package, lib.loc = NULL)
codocData(package, lib.loc = NULL)
}
\description{
  Find inconsistencies between actual and documented \sQuote{structure}
  of \R objects in a package.  \code{codoc} compares names and
  optionally also corresponding positions and default values of the
  arguments of functions.  \code{codocClasses} and \code{codocData}
  compare slot names of S4 classes and variable names of data sets,
  respectively.
}
\arguments{
  \item{package}{a character string naming an installed package.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This must contain the subdirectories \file{man}
    with \R documentation sources (in Rd format) and \file{R} with \R
    code.  Only used if \code{package} is not given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
  \item{use.values}{if \code{FALSE}, do not use function default values
    when comparing code and docs.  Otherwise, compare \emph{all} default
    values if \code{TRUE}, and only the ones documented in the usage
    otherwise (default).}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
}
\note{
  The default for \code{use.values} has been changed from
  \code{FALSE} to \code{NULL}, for \R versions 1.9.0 and later.
}
\details{
  The purpose of \code{codoc} is to check whether the documented usage
  of function objects agrees with their formal arguments as defined in
  the \R code.  This is not always straightforward, in particular as the
  usage information for methods to generic functions often employs the
  name of the generic rather than the method.

  The following algorithm is used.  If an installed package is used, it
  is loaded (unless it is the \pkg{base} package), after possibly
  detaching an already loaded version of the package.  Otherwise, if the
  sources are used, the \R code files of the package are collected and
  sourced in a new environment.  Then, the usage sections of the Rd
  files are extracted and parsed \sQuote{as much as possible} to give
  the formals documented.  For interpreted functions in the code
  environment, the formals are compared between code and documentation
  according to the values of the argument \code{use.values}.  Synopsis
  sections are used if present; their occurrence is reported if
  \code{verbose} is true.

  If a package has a namespace both exported and unexported objects are
  checked, as well as registered S3 methods.  (In the unlikely event of
  differences the order is exported objects in the package, registered
  S3 methods and finally objects in the namespace and only the first
  found is checked.)

  Currently, the R documentation format has no high-level markup for the
  basic \sQuote{structure} of classes and data sets (similar to the usage
  sections for function synopses).  Variable names for data frames in
  documentation objects obtained by suitably editing \sQuote{templates}
  created by \code{\link{prompt}} are recognized by \code{codocData}
  and used provided that the documentation object is for a single data
  frame (i.e., only has one alias).  \code{codocClasses} analogously
  handles slot names for classes in documentation objects obtained by
  editing shells created by \code{\link{promptClass}}.

  Help files named \file{\var{pkgname}-defunct.Rd} for the
  appropriate \var{pkgname} are checked more loosely, as they may
  have undocumented arguments.
}
\value{
  \code{codoc} returns an object of class \code{"codoc"}.  Currently,
  this is a list which, for each Rd object in the package where an
  inconsistency was found, contains an element with a list of the
  mismatches (which in turn are lists with elements \code{code} and
  \code{docs}, giving the corresponding arguments obtained from the
  function's code and documented usage).

  \code{codocClasses} and \code{codocData} return objects of class
  \code{"codocClasses"} and \code{"codocData"}, respectively, with a
  structure similar to class \code{"codoc"}.

  There are \code{print} methods for nicely displaying the information
  contained in such objects.
}
\seealso{
  \code{\link{undoc}}, \code{\link{QC}}
}
\keyword{documentation}
