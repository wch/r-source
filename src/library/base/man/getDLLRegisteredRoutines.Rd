% File src/library/base/man/getDLLRegisteredRoutines.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{getDLLRegisteredRoutines}
\alias{getDLLRegisteredRoutines}
\alias{getDLLRegisteredRoutines.character}
\alias{getDLLRegisteredRoutines.DLLInfo}
\alias{print.NativeRoutineList}
\alias{print.DLLRegisteredRoutines}
\title{Reflectance Information for C/Fortran routines in a DLL}
\description{
  This function allows us to query the set of routines
  in a DLL that are registered with R to enhance
  dynamic lookup, error handling when calling native routines,
  and potentially security in the future.
  This function provides a description of each of the
  registered routines in the DLL for the different interfaces,
  i.e.\sspace{}\code{\link{.C}}, \code{\link{.Call}}, \code{\link{.Fortran}}
  and \code{\link{.External}}.
}
\usage{
getDLLRegisteredRoutines(dll, addNames = TRUE)
}
\arguments{
  \item{dll}{a character string or \code{DLLInfo} object.
    The character string specifies the file name of the DLL
    of interest, and is given without the file name extension (e.g., the
    \file{.dll} or \file{.so}) and with no directory/path information.
    So a file \file{MyPackage/libs/MyPackage.so} would be specified as
    \samp{MyPackage}.

    The \code{DLLInfo} objects can be obtained directly
    in calls to \code{\link{dyn.load}} and  \code{\link{library.dynam}},
    or can be found after the DLL has been loaded using
    \code{\link{getLoadedDLLs}}, which returns a list of
    \code{DLLInfo} objects (index-able by DLL file name).

    The \code{DLLInfo} approach avoids any ambiguities related to two
    DLLs having the same name but corresponding to files in different
    directories.
  }
  \item{addNames}{a logical value.  If this is \code{TRUE}, the elements
    of the returned lists are named using the names of the routines (as
    seen by R via registration or raw name).  If \code{FALSE},
    these names are not computed and assigned to the lists.  As a
    result, the call should be quicker.  The name information is also
    available in the \code{NativeSymbolInfo} objects in the lists.
  }
}
\details{
  This takes the registration information after it has been registered
  and processed by the R internals.  In other words, it uses the extended
  information.

  There is \code{print} methods for the class, which prints only the
  types which have registered routines.
}
\value{
  A list of class \code{"DLLRegisteredRoutines"} with four elements
  corresponding to the routines registered for the \code{.C},
  \code{.Call}, \code{.Fortran} and \code{.External} interfaces. Each is
  a list with as many elements as there were routines registered for
  that interface.

  Each element identifies a routine and is an object
  of class \code{"NativeSymbolInfo"}.
  An object of this class has the following fields:
  \item{name}{the registered name of the routine (not necessarily the
    name in the C code).}
  \item{address}{the memory address of the routine as resolved in the
    loaded DLL. This may be \code{NULL} if the symbol has not yet been
    resolved.}
  \item{dll}{an object of class \code{DLLInfo} describing the DLL.
    This is same for all elements returned.}
  \item{numParameters}{the number of arguments the native routine is to
    be called with.  In the future, we will provide information about
    the types of the parameters also.
  }
}
\references{
  \sQuote{Writing R Extensions Manual} for symbol registration.

  R News, Volume 1/3, September 2001.   "In search of C/C++ & Fortran Symbols"
}
\author{Duncan Temple Lang \email{duncan@wald.ucdavis.edu}}

\seealso{
  \code{\link{getLoadedDLLs}},
  \code{\link{getNativeSymbolInfo}} for information on the entry points listed.
}
\examples{
dlls <- getLoadedDLLs()
getDLLRegisteredRoutines(dlls[["base"]])

getDLLRegisteredRoutines("stats")
}
\keyword{interface}

