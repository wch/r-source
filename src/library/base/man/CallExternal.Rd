% File src/library/base/man/Foreign.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{CallExternal}
\alias{.Call}
\alias{.External}
\title{Modern Interfaces to C/C++ code}
\description{
  Functions to pass \R objects to compiled C/C++ code that has been
  loaded into \R.
}
\usage{
    .Call(.NAME, \dots, PACKAGE)
.External(.NAME, \dots, PACKAGE)
}
\arguments{
  \item{.NAME}{a character string giving the name of a C function,
    or an object of class \code{"\link{NativeSymbolInfo}"},
    \code{"\link{RegisteredNativeSymbol}"} or
    \code{"\link{NativeSymbol}"} referring to such a name.}

  \item{\dots}{arguments to be passed to the compiled code. Up to 65 for
    \code{.Call}.}

  \item{PACKAGE}{if supplied, confine the search for a character string
    \code{.NAME} to the DLL given by this argument (plus the
    conventional extension, \file{.so}, \file{.dll}, \dots).

    This argument follows \code{...} and so its name cannot be abbreviated.

    This is intended to add safety for packages, which can ensure by
    using this argument that no other package can override their
    external symbols, and also speeds up the search (see \sQuote{Note}).}
}

\details{
  The functions are used to call compiled code which makes use of
  internal \R objects, passing the arguments to the code as a sequence
  of \R objects.  They assume C calling conventions, so can usually
  also be used of C++ code.

  For details about how to write code to use with these functions see
  the chapter on \sQuote{System and foreign language interfaces} in
  the \sQuote{Writing \R Extensions} manual.  They differ in the way the
  arguments are passed to the C code:  \code{.External} allows for a
  variable number of arguments.

  These functions are \link{primitive}, and \code{.NAME} is always
  matched to the first argument supplied (which should not be named).
  For clarity, avoid using names in the arguments passed to \code{\dots}
  that match or partially match \code{.NAME}.
}

\value{
  An \R object constructed in the compiled code.
}

\section{Header files for external code}{
  Writing code for use with these functions will need to use internal \R
  structures defined in \file{Rinternals.h} and/or the macros in
  \file{Rdefines.h}.
}

\note{
  If one of these functions is to be used frequently, do specify
  \code{PACKAGE} (to confine the search to a single DLL) or pass
  \code{.NAME} as one of the native symbol objects.  Searching for
  symbols can take a long time, especially when many namespaces are loaded.

  You may see \code{PACKAGE = "base"} for symbols linked into \R.  Do
  not use this in your own code: such symbols are not part of the API
  and may be changed without warning.

  \code{PACKAGE = ""} used to be accepted (but was undocumented): it is
  now an error.
}

\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer. (\code{.Call}.)
}
\seealso{
  \code{\link{dyn.load}}, \code{\link{.C}}, \code{\link{.Fortran}}.

  The \sQuote{Writing R Extensions} manual.
}
\keyword{programming}
