\name{dyn.load}
\alias{dyn.load}
\alias{dyn.unload}
\alias{is.loaded}
\alias{symbol.C}
\alias{symbol.For}
\title{Foreign Function Interface}
\description{
  Load or unload shared libraries, and test whether a C function or
  Fortran subroutine is available.
}
\usage{
dyn.load(x, local = TRUE, now = TRUE)
dyn.unload(x)

is.loaded(symbol, PACKAGE = "")
symbol.C(name)
symbol.For(name)
}
\arguments{
  \item{x}{a character string giving the pathname to a shared library or
    DLL.}
  \item{local}{a logical value controlling whether the symbols in the
    shared library are stored in their own local table and not shared
    across shared libraries,
    or added to the global symbol table.  Whether this has any effect is
    system-dependent.
#ifdef windows
    It is ignored on Windows.
#endif
  }
  \item{now}{a logical controlling whether all symbols are resolved (and
    relocated) immediately the library is loaded or deferred until they
    are used.  This control is useful for developers testing whether a
    library is complete and has all the necessary symbols, and for users
    to ignore missing symbols. Whether this has any effect is system-dependent.
#ifdef windows
    It is ignored on Windows.
#endif
  }
  \item{symbol}{a character string giving a symbol name.}
  \item{PACKAGE}{if supplied, confine the search for the \code{name} to
    the DLL given by this argument (plus the conventional extension,
    \code{.so}, \code{.sl}, \code{.dll}, \dots).  This is intended to
    add safety for packages, which can ensure by using this argument
    that no other package can override their external symbols.  Use
    \code{PACKAGE="base"} for symbols linked in to \R.  This is used in the
    same way as in \code{\link{.C}}, \code{\link{.Call}},
    \code{\link{.Fortran}} and \code{\link{.External}} functions}
  \item{name}{a character string giving either the name of a C function
    or Fortran subroutine.  Fortran names probably need to be given
    entirely in lower case (but this may be system-dependent).}
}
\details{
  See
#ifdef windows
  file \file{README.packages},
#endif
  \sQuote{See Also} and the \emph{Writing R Extensions} manual for how to
  create a suitable shared library.  Note that unlike some versions of
  S-PLUS, \code{dyn.load} does not load an object (\code{.o}) file but a
  shared library or DLL.

  Unfortunately a very few platforms (Compaq Tru64) do not handle the
  \code{PACKAGE} argument correctly, and may incorrectly find symbols
  linked into \R.
  
  The additional arguments to \code{dyn.load} mirror the different
  aspects of the mode argument to the dlopen() routine on UNIX
  systems. They are available so that users can exercise greater control
  over the loading process for an individual library.  In general, the
  defaults values are appropriate and you should override them only if
  there is good reason and you understand the implications.

#ifdef unix
  The \code{local} argument allows one to control whether the symbols in
  the DLL being attached are visible to other DLLs.  While maintaining
  the symbols in their own namespace is good practice, the ability to
  share symbols across related \dQuote{chapters} is useful in many
  cases.  Additionally, on certain platforms and versions of an
  operating system, certain libraries must have their symbols loaded
  globally to successfully resolve all symbols.

  One should be careful of the potential side-effect of using lazy
  loading via the \code{now} argument as \code{FALSE}.  If a routine is
  called that has a missing symbol, the process will terminate
  immediately and unsaved session variables will be lost.  The intended
  use is for library developers to call specify a value \code{TRUE} to
  check that all symbols are actually resolved and for regular users to
  all with \code{FALSE} so that missing symbols can be ignored and the
  available ones can be called.

  The initial motivation for adding these was to avoid such termination
  in the \code{_init()} routines of the Java virtual machine library.
  However, symbols loaded locally may not be (read probably) available
  to other DLLs.  Those added to the global table are available to all
  other elements of the application and so can be shared across two
  different DLLs.

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
#endif

#ifdef windows
  External code must not change the floating point control word, but
  many DLLs do so.  Common changes are to set it to use 53 bit 
  precision instead of R's default 64 bit precision, or to unmask 
  some exceptions.  \code{dyn.load} detects such changes, 
  and restores R's control word to its default value of hex 8001F.  
  This may cause the DLL to malfunction; if so, it should be rewritten
  to save and restore the control word itself.  If \code{warn.FPU}
  is set to \code{TRUE} using the \code{\link{options}} function, 
  a warning will be printed.  (If the warning says
  that the control word was changed from some other value than 8001F, 
  please report the circumstances to the Windows maintainers:  
  that probably indicates an internal bug.)
#endif
}
\value{
  The function \code{dyn.load} is used for its side effect which links
  the specified shared library to the executing \R image.  Calls to
  \code{.C}, \code{.Call}, \code{.Fortran} and \code{.External} can
  then be used to execute compiled C functions or Fortran subroutines
  contained in the library. The return value of \code{dyn.load} is
  an object of class \code{DLLInfo}. See \code{\link{getLoadedDLLs}}
  for information about this class.

  The function \code{dyn.unload} unlinks the shared library.

  Functions \code{symbol.C} and \code{symbol.For} map function or
  subroutine names to the symbol name in the compiled code:
  \code{is.loaded} checks if the symbol name is loaded and hence
  available for use in \code{.C} or \code{.Fortran}.
}
\note{
  The creation of shared libraries and the runtime linking of them into
  executing programs is very platform dependent.  In recent years there
  has been some simplification in the process because the C subroutine
  call \code{dlopen} has become the standard for doing this under UNIX.
  Under UNIX \code{dyn.load} uses the \code{dlopen} mechanism and should
  work on all platforms which support it.  On Windows it uses the
  standard mechanisms for loading 32-bit DLLs.

  The original code for loading DLLs in UNIX was provided by Heiner
  Schwarte.  The compatibility code for HP-UX was provided by Luke
  Tierney.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{library.dynam}} to be used inside a package's
  \code{\link{.First.lib}} initialization.

#ifdef unix
  \code{\link{SHLIB}} for how to create suitable shared objects.
#endif
#ifdef windows
  \code{\link{SHLIB}} for how to create suitable DLLs.
#endif
  
  \code{\link{.C}},
  \code{\link{.Fortran}},
  \code{\link{.External}},
  \code{\link{.Call}}.
}
\examples{
is.loaded(symbol.For("hcass2")) #-> probably TRUE, as stats is loaded
}
\keyword{interface}
