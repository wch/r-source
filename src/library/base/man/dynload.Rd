\name{dyn.load}
\title{Foreign Function Interface}
\usage{
dyn.load(libname)

is.loaded(symbol)
symbol.C(name)
symbol.For(name)
}
\alias{dyn.load}
\alias{is.loaded}
\alias{symbol.C}
\alias{symbol.For}
\arguments{
\item{libname}{a character string giving the pathname to a DLL.}
\item{symbol}{a character string giving a symbol name.}
\item{name}{a character string giving either the name of a C
function or Fortran subroutine.}
}
\value{
The function \code{dyn.load} is used for its side effect which
links the specified shared library to the executing \R image.
Calls to \code{.C} and \code{.Fortran} can then be used to execute
C functions or Fortran subroutines contained in the library.
}
\note{
The creation of shared libraries the runtime linking of them
into executing programs is very platform dependent.
In recent years there has been some simplification in the process
because the C subroutine call \code{dlopen} has become the standard
for doing this under Unix.
\code{dyn.load} uses the \code{dlopen} mechanism and
should work on all platforms which support it.

The original code for loading DLLs in UNIX
was provided by Heiner Schwarte.
The compatibility code for HP-UX was provided by Luke Tierney.
}
\seealso{
\code{\link{.C}}, \code{\link{.Fortran}}.
}
\examples{
is.loaded(symbol.For("hcass2")) #-> probably  FALSE
library(mva)
is.loaded(symbol.For("hcass2")) #-> TRUE
}
\keyword{interface}
