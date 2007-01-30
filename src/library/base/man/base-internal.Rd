\name{base-internal}
\alias{.subset}
\alias{.subset2}
\alias{.find.package}
\alias{.path.package}
\alias{.getRequiredPackages}
\alias{.getRequiredPackages2}
\alias{testPlatformEquivalence}
\alias{.isMethodsDispatchOn}
\alias{sys.save.image}
\alias{sys.load.image}
\alias{.row_names_info}
\alias{.set_row_names}
\alias{.ArgsEnv}
\alias{.GenericArgsEnv}
\title{Internal Functions in the Base Package}
\description{
  Internal functions in the base package, which are only user-visible
  because of the special nature of the base name space.
}
\usage{
.subset(x, \dots)
.subset2(x, \dots)

.find.package(package, lib.loc = NULL, quiet = FALSE,
              verbose = getOption("verbose"))

.path.package(package, quiet = FALSE)

.getRequiredPackages(file = "DESCRIPTION", quietly = FALSE,
                     useImports = FALSE)
.getRequiredPackages2(pkgInfo, quietly = FALSE, lib.loc = NULL,
                      useImports = FALSE)

testPlatformEquivalence(built, run)

.isMethodsDispatchOn(onOff = NULL)

sys.load.image(name, quiet)
sys.save.image(name)

.row_names_info(x, type = 1L)
.set_row_names(n)
}
\arguments{
  \item{x}{object from which to extract elements.}
  \item{package}{the name of a package.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{quiet}{logical.  Should this not give warnings or an error
    if the package(s) are not found?}
  \item{useImports}{logical.  Should  the \code{Imports} field be processed?}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{file}{The full path to the \file{DESCRIPTION} file of a package.}
  \item{quietly}{logical: should message(s) be printed.}
  \item{pkginfo}{A processed \file{DESCRIPTION} file of class
    \code{"packageDescription2"}.}
  \item{built, run}{platform strings of the form \code{"cpu-vendor-os"}.}
  \item{name}{name of image file to save or load.}
  \item{x}{an object with a \code{"row.names"} attribute, typically a
    data frame.}
  \item{type}{integer. Currently \code{type = 0} returns the internal
    \code{"row.names"} attribute (possibly \code{NULL}), \code{type = 2}
    the number of rows implied by the attribute, and \code{type = 1} the
    latter with a negative sign for \sQuote{automatic} row names.}
  \item{n}{integer.  The number of rows.}
}
\details{
  The functions \code{.subset} and \code{.subset2} are essentially
  equivalent to the \code{\link{[}} and \code{\link{[[}} operators,
  except methods that dispatch does not take place.  This is to avoid
  expensive unclassing when applying the default method to an object.  They
  should not normally be invoked by end users.  Note that unlike the
  operators they are builtins and not specials and hence do not allow
  missing values in their arguments.

  \code{.find.package} returns the paths to the locations where the
  given packages can be found.  If \code{lib.loc} is \code{NULL}, then
  then attached packages are searched before the libraries.  If a
  package is found more than once, the first match is used.  Unless
  \code{quiet = TRUE} a warning will be given about the named packages
  which are not found, and an error if none are.  If \code{verbose} is
  true, warnings about packages found more than once are given.  It does
  know about versioned installs of packages, and for a package to be
  returned it must have a \file{DESCRIPTION} file containing a valid
  \code{version} field.  Where more than one versioned match is found to
  a unversioned name (and no exact match), the highest-numbered version
  is selected.

  \code{.path.package} returns the paths from which the named packages
  were loaded, or if none were named, for all currently loaded packages.
  Unless \code{quiet = TRUE} it will warn if some of the packages named
  are not loaded, and given an error if none are.  This function is not
  meant to be called by users, and its interface might change in future
  versions.  Note that currently \code{package} must include the
  version number if versioned installs are used.

  \code{.getRequiredPackages2} attaches all the packages mentioned in the
  \code{Depends} field: failure to find a package is an error.  It also
  checks the versions of the packages found against the \code{Depends} field.

  \code{.getRequiredPackages} is a wrapper to
  \code{.getRequiredPackages2} using a \file{DESCRIPTION} file.

  \code{testPlatformEquivalence} is used to test if a package with 
  compiled code can safely be loaded into a build of \R, which it does by
  comparing their \sQuote{canonical} \code{"cpu-vendor-os"} descriptions.
  The default function omits the \code{vendor} part and allows small 
  mismatches elsewhere.  Cognoscenti can replace this function in the
  \code{base} name space.

  The function \code{.isMethodsDispatchOn()} returns \code{TRUE} if
  the S4 method dispatch has been turned on in the evaluator (usually by
  loading package \pkg{methods}).  It is meant for \R internal use only.

  \code{sys.save.image} is a system function that is called by \code{q()}
  and its GUI analogs; \code{sys.load.image} is called by the startup code.
  These functions should not be called directly and are subject to change.

  \code{sys.save.image} closes all connections first, to ensure that it
  is able to open a connection to save the image.  This is appropriate
  when called from \code{q()} and allies, but reinforces the warning
  that it should not be called directly.

  \code{\link{row.names}} can be stored internally in compact form.
  \code{.set_row_names(n)} generates that form for automatic row names
  of length \code{n}, to be assigned to
  \code{attr(<a data frame>, "row.names")}.  \code{.row_names_info}
  gives information on the internal form of the row names for a data
  frame: for details of what information see the argument \code{type}.

  \code{.GenericArgsEnv} and \code{.ArgsEnv} are environments that
  contain closures with the argument lists that the primitives would
  have had had they been closures.  All the primitives that are internal
  S3 generics have corresponding members of \code{.GenericArgsEnv} and
  the remaining non-language-element primitives correspond to
  \code{.ArgsEnv}. See the \sQuote{R Internals} manual for further
  details.  They are used by \code{\link{args}} and
  \code{\link{print.default}} and the QC functions \code{\link{codoc}}
  and \code{\link{checkS3methods}}.
}
\value{
  \code{.find.package} and \code{.path.packages} return a character
  vector of paths to installed packages.
  
  \code{.getRequiredPackages} and \code{.getRequiredPackages2}
  return \code{invisible()}.

  \code{testPlatformEquivalence}: \code{TRUE} or \code{FALSE}.
}
\keyword{internal}
