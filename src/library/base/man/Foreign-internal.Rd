% File src/library/base/man/Foreign-internal.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{Foreign-internal}
\alias{.External.graphics}
\alias{.External2}
\alias{.Call.graphics}
\title{Internal Versions of the Foreign Function Interface}
\description{
  Internal versions of \code{\link{.Call}} and \code{\link{.External}}.
}
\usage{
    .Call.graphics(.NAME, \dots, PACKAGE)
.External.graphics(.NAME, \dots, PACKAGE)
        .External2(.NAME, \dots, PACKAGE)
}
\arguments{
  \item{.NAME}{a character string giving the name of a C function or
    Fortran subroutine, or an object of class
    \code{"\link{NativeSymbolInfo}"}, \code{"\link{RegisteredNativeSymbol}"}
    or \code{"\link{NativeSymbol}"} referring to such a name.}
  \item{\dots}{arguments to be passed to the foreign function.}
  \item{PACKAGE}{see the corresponding argument of \code{\link{.Call}}.}
}
\details{
  The \code{.graphics} versions of \code{.Call} and \code{.External}
  are used when calling code which makes low-level graphics calls.
  They take additional steps to ensure that the device driver display
  lists are updated correctly.

  \code{.External2} is an enhanced version of \code{.External} which
  passes additional information in more arguments.

  \code{.Call.graphics} is used in package \pkg{grid} and
  \code{.External.graphics} and \code{.External2} are used in package
  \code{graphics}.

  These are primitive functions.
}
\value{
  An \R object.
}
\seealso{\code{\link{.Call}}.}
\keyword{internal}
