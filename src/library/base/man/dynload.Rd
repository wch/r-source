\name{dynload}
\title{Foreign Function Interface}
\usage{
dyn.load(libname)
dyn.unload(libname)

is.loaded(symbol)
symbol.C(name)
symbol.For(name)
}
\alias{dyn.load}
\alias{dyn.unload}
\alias{is.loaded}
\alias{symbol.C}
\alias{symbol.For}
\arguments{
\item{libname}{a character string giving the pathname to a DLL.}
\item{symbol}{a character string giving a symbol name.}
\item{name}{a character string giving either the name of a C
function or Fortran subroutine. Fortran names probably need to be given
entirely in lower case (but this may be system-dependent).}
}
\value{
The function \code{dyn.load} is used for its side effect which
links the specified shared library to the executing \R image.
Calls to \code{.C}, \code{.Fortran} and \code{.External} can
then be used to execute compiled C functions or Fortran subroutines
contained in the library.

The function \code{dyn.unload} unlinks the shared library.

Functions \code{symbol.C} and \code{symbol.For} map function /
subroutine names to
the symbol name in the compiled code: \code{is.loaded} checks if the
symbol name is loaded and so available for use in \code{.C} \dots.
}
\note{
The creation of shared libraries and the runtime linking of them
into executing programs is very platform dependent.
In recent years there has been some simplification in the process
because the C subroutine call \code{dlopen} has become the standard
for doing this under UNIX.
Under UNIX \code{dyn.load} uses the \code{dlopen} mechanism and
should work on all platforms which support it. On Windows it uses
the standard mechanisms for loading 32-bit DLLs.

The original code for loading DLLs in UNIX
was provided by Heiner Schwarte.
The compatibility code for HP-UX was provided by Luke Tierney.
}
\seealso{
\code{\link{.C}}, \code{\link{.Fortran}}, \code{\link{.External}}.
}
\examples{
is.loaded(symbol.For("hcass2")) #-> probably  FALSE
library(mva)
is.loaded(symbol.For("hcass2")) #-> TRUE
}
\keyword{interface}
