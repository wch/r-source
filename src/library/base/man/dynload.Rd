\name{dynload}
\title{Foreign Function Interface}
\usage{
dyn.load(libname, local=TRUE, now=TRUE)
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
\item{local}{a logical value controlling whether the symbols in the DLL are stored
in their own local table and not shared across DLLs,
or added to the global symbol table. Whether this has any
effect is system-dependent.}
\item{now}{a logical controlling whether all symbols are resolved (and relocated)
immediately the library is loaded or deferred until they
are used. This control is useful for developers testing
whether a library is complete and has all the necessary
symbols and for users to ignore missing symbols.}
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


The additional arguments to \code{dyn.load} mirror the different
aspects of the mode argument to the dlopen() routine on UNIX
systems. They are available so that users can exercise greater control
over the loading process for an individual library. In general, the
defaults values are appropriate and one should override them only if
there is good reason and you understand the implications.

The \code{local} argument allows one to control whether the symbols in
the DLL being attached are visible to other DLLs.  While maintaining
the symbols in their own namespace is good practice, the ability to
share symbols across related ``chapters'' is useful in many cases.
Additionally, on certain platforms and versions of an operating
system, certain libraries must have their symbols loaded globally to
successfully resolve all symbols.

One should be careful of the potential side-effect of using lazy
loading via the `now' argument as `FALSE'. If a routine is called that
has a missing symbol, the process will terminate immediately and
unsaved session variables will be lost. The intended use is for
library developers to call specify a value `TRUE' to check that all
symbols are actually resolved and for regular users to all with
`FALSE' so that missing symbols can be ignored and the available ones
can be called.

The initial motivation for adding these was to avoid such termination
in the \code{_init()} routines of the Java virtual machine
library. However, symbols loaded locally may not be (read probably)
available to other DLLs. Those added to the global table are available
to all other elements of the application and so can be shared across
two different DLLs.

Some systems do not provide (explicit) support for local/global and
lazy/eager symbol resolution.  This can be the source of subtle bugs.
One can arrange to have warning messages emitted when unsupported
options are used.  This is done by setting either of the options
\code{verbose} or \code{warn} to be non-zero via the
\code{\link{options}} function.  Currently, we know of only 2
platforms that do not provide a value for local load (RTLD\_LOCAL).
These are IRIX6.4 and unpatched versions of Solaris 2.5.1.

There is a short discussion of these additional arguments with some
example code available at
\url{http://cm.bell-labs.com/stat/duncan/R/dynload}.
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
  \code{\link{library.dynam}} to be used inside a package's
  \code{\link{.First.lib}} initialization.

  \code{\link{.C}}, \code{\link{.Fortran}}, \code{\link{.External}},
  \code{\link{.Call}}.
}
\examples{
is.loaded(symbol.For("hcass2")) #-> probably  FALSE
library(mva)
is.loaded(symbol.For("hcass2")) #-> TRUE
}
\keyword{interface}
