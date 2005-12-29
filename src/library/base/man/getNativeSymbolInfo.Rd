\name{getNativeSymbolInfo}
\alias{getNativeSymbolInfo}
\title{Obtain a description of one or more native (C/Fortran) symbols}
\description{
  This finds and returns as comprehensive a description of one or more
  dynamically loaded or \dQuote{exported} built-in native
  symbols. For each name, it returns information about the
  name of the symbol, the library in which it is located
  and, if available, the number of arguments it expects
  and by which interface it should be called (i.e
  \code{\link{.Call}},   \code{\link{.C}},
  \code{\link{.Fortran}},  or \code{\link{.External}}).
  Additionally, it returns the address of the symbol and this
  can be passed to other C routines which can invoke.
  Specifically, this provides a way to explicitly share
  symbols between different dynamically loaded package libraries.
  Also, it provides a way to query where symbols were resolved,
  and aids diagnosing strange behavior associated with dynamic
  resolution.

  This is now vectorized in the \code{name} argument so can process
  multiple symbols in a single call. The result is a list that can be
  indexed by the given symbol names.
}
\usage{
getNativeSymbolInfo(name, PACKAGE, unlist = TRUE,
                     withRegistrationInfo = FALSE)
}
\arguments{
  \item{name}{the name of the native symbol as used in a call
    to \code{\link{is.loaded}}, etc.  Note that Fortran symbols should be
    supplied as-is, not wrapped in \code{symbol.For}.
  }
  \item{PACKAGE}{an optional argument that specifies to which
    dynamically loaded library we restrict the search for this symbol.
    If this is \code{"base"}, we search in the R executable itself.}
  \item{unlist}{a logical value which controls how the result is
    returned if the function is called with the name of a single symbol.
    If \code{unlist} is \code{TRUE} and the number of symbol names in
    \code{name} is one, then the \code{NativeSymbolInfo} object
    is returned. If it is \code{FALSE}, then the list
    of \code{NativeSymbolInfo} objects is returned.
    This is ignored if the number of symbols passed in \code{name} is
    more than one. 
    To be compatible with earlier versions of this function, this
    defaults to \code{TRUE}.
  }
  \item{withRegistrationInfo}{a logical value indicating whether,
    if \code{TRUE}, to
    return information that was registered with R about the symbol
    and its parameter types if such information is available,
    or if \code{FALSE} to return the address of the 
  }
}
\details{
  This uses the same mechanism for resolving symbols as is used
  in all the native interfaces (\code{\link{.Call}}, etc.).
  If the symbol has been explicitly registered by the shared library
  in which it is contained, information about the number of arguments
  and the interface by which it should be called will be returned.
  Otherwise, a generic native symbol object is returned.
}
\value{
  Generally, a list of \code{NativeSymbolInfo} elements
  whose elements can be indexed by the elements of
  \code{name}  in the call.
  Each \code{NativeSymbolInfo} object is a list containing the following elements:
  \item{name}{the name of the symbol, as given by the
  \code{name} argument.}
\item{address}{if \code{withRegistrationInfo} is \code{FALSE},
    this is the native memory address of the symbol which can
    be used to invoke the routine, and also
    compare with other symbol address. This is an external pointer
    object and of class \code{NativeSymbol}.
    If \code{withRegistrationInfo} is \code{TRUE} and registration
    information is available for the symbol, then this is
    an object of class \code{RegisteredNativeSymbol} and is a reference
    to an internal data type that has access to the routine pointer and
    registration information. This too can be used in calls to
    \code{\link{.Call}}, \code{\link{.C}}, \code{\link{.Fortran}} and
    \code{\link{.External}}.
  }
  \item{package}{a list containing 3 elements:
    \describe{
      \item{name}{the short form of the library name which can be used
	as the value of the \code{PACKAGE} argument in
	the different native interface functions.}
      \item{path}{the fully qualified name of the shared library file.}
      \item{dynamicLookup}{a logical value indicating whether dynamic
	resolution is used when looking for symbols in this library,
	or only registered routines can be located.}
    }    
  }
  If the routine was explicitly registered by the dynamically
  loaded library, the list contains a fourth field
  \item{numParameters}{the number of arguments that should be passed
    in a call to this routine.}
  Additionally, the list will have an additional class,
  being \code{CRoutine},   \code{CallRoutine},
  \code{FortranRoutine} or \code{ExternalRoutine}
  corresponding to the R interface by which it should be invoked.

  If any of the symbols is not found, an error is immediately raised.

  If \code{name} contains only one symbol name and \code{unlist} is
  \code{TRUE}, then the single \code{NativeSymbolInfo} is returned
  rather than the list containing that one element.
}
\references{
  For information about registering native routines,
  see \dQuote{In Search of {C/C++} \& {FORTRAN} Routines},
  R News, volume 1, number 3, 2001, p20--23
  (\url{http://CRAN.R-project.org/doc/Rnews/}).  
}
\author{Duncan Temple Lang}
\note{
  One motivation for accessing this reflectance information
  is to be able to pass native routines to 
  C routines as \dQuote{function pointers} in C.
  This allows us to treat native routines and R functions
  in a similar manner, such as when passing an R function
  to C code that makes callbacks to that function
  at different points in its computation
  (e.g., \code{\link{nls}}).
  Additionally, we can resolve the symbol just once and
  avoid resolving it repeatedly or using the internal
  cache.
  In the future, one may be able to treat \code{NativeSymbol}
  objects directly as callback objects.
}
\seealso{
  \code{\link{getDLLRegisteredRoutines}},
  \code{\link{is.loaded}},
  \code{\link{.C}},
  \code{\link{.Fortran}},
  \code{\link{.External}},
  \code{\link{.Call}},
  \code{\link{dyn.load}}.  
}

\examples{
library(stats) # normally loaded
getNativeSymbolInfo("dansari")

getNativeSymbolInfo("hcass2")  # a Fortran symbol
}
\keyword{interface}
