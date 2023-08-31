% File src/library/base/man/dynload.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{dyn.load}
\alias{dyn.load}
\alias{dyn.unload}
\alias{is.loaded}
\alias{R_MAX_NUM_DLLS}
#ifdef windows
\alias{DLLpath}
#endif
\title{Foreign Function Interface}
\description{
  Load or unload DLLs (also known as shared objects), and test whether a
  C function or Fortran subroutine is available.
}
\usage{
dyn.load(x, local = TRUE, now = TRUE, ...)
dyn.unload(x)

is.loaded(symbol, PACKAGE = "", type = "")
}
\arguments{
  \item{x}{a character string giving the pathname to a DLL, also known
    as a dynamic shared object.  (See \sQuote{Details} for what these
    terms mean.)}
  \item{local}{a logical value controlling whether the symbols in the
    DLL are stored in their own local table and not shared
    across DLLs, or added to the global symbol table.  Whether this has
    any effect is system-dependent.
#ifdef windows
    It is ignored on Windows.
#endif
  }
  \item{now}{a logical controlling whether all symbols are resolved (and
    relocated) immediately when the library is loaded or deferred until they
    are used.  This control is useful for developers testing whether a
    library is complete and has all the necessary symbols, and for users
    to ignore missing symbols.  Whether this has any effect is system-dependent.
#ifdef windows
    It is ignored on Windows.
#endif
  }
  \item{\dots}{other arguments for future expansion.
#ifdef windows
    See section \sQuote{Windows} below.
#endif
  }
  \item{symbol}{a character string giving a symbol name.}
  \item{PACKAGE}{if supplied, confine the search for the \code{name} to
    the DLL given by this argument (plus the conventional extension,
    \file{.so}, \file{.sl}, \file{.dll}, \dots).  This is intended to
    add safety for packages, which can ensure by using this argument
    that no other package can override their external symbols.  This is
    used in the same way as in the \code{\link{.C}}, \code{\link{.Call}},
    \code{\link{.Fortran}} and \code{\link{.External}} functions.}
  \item{type}{The type of symbol to look for: can be any (\code{""}, the
    default), \code{"Fortran"}, \code{"Call"} or \code{"External"}.}
}
\details{
  The objects \code{dyn.load} loads are called \sQuote{dynamically
    loadable libraries} (abbreviated to \sQuote{DLL}) on all platforms
  except macOS, which uses the term for a different sort
  of object.  On Unix-alikes they are also called \sQuote{dynamic
    shared objects} (\sQuote{DSO}), or \sQuote{shared objects} for
  short.  (The POSIX standards use \sQuote{executable object file},
  but no one else does.)

  See \sQuote{See Also} and the \sQuote{Writing R Extensions} and
  \sQuote{R Installation and Administration} manuals for how to create
  and install a suitable DLL.

  Unfortunately some rare platforms (e.g., Compaq Tru64) do not handle
  the \code{PACKAGE} argument correctly, and may incorrectly find
  symbols linked into \R.

  The additional arguments to \code{dyn.load} mirror the different
  aspects of the mode argument to the \code{dlopen()} routine on POSIX
  systems.  They are available so that users can exercise greater control
  over the loading process for an individual library.  In general, the
  default values are appropriate and you should override them only if
  there is good reason and you understand the implications.

#ifdef unix
  The \code{local} argument allows one to control whether the symbols in
  the DLL being attached are visible to other DLLs.  While maintaining
  the symbols in their own namespace is good practice, the ability to
  share symbols across related \sQuote{chapters} is useful in many
  cases.  Additionally, on certain platforms and versions of an
  operating system, certain libraries must have their symbols loaded
  globally to successfully resolve all symbols.

  One should be careful of one potential side-effect of using lazy
  loading via \code{now = FALSE}: if a routine is
  called that has a missing symbol, the process will terminate
  immediately.  The intended use is for library developers to call this with
  value \code{TRUE} to check that all symbols are actually resolved and
  for regular users to call it with \code{FALSE} so that missing symbols
  can be ignored and the available ones can be called.

  The initial motivation for adding these was to avoid such termination
  in the \code{_init()} routines of the Java virtual machine library.
  However, symbols loaded locally may not be (read: probably) available
  to other DLLs.  Those added to the global table are available to all
  other elements of the application and so can be shared across two
  different DLLs.

  Some (very old) systems do not provide (explicit) support for
  local/global and lazy/eager symbol resolution.  This can be the source
  of subtle bugs.  One can arrange to have warning messages emitted when
  unsupported options are used.  This is done by setting either of the
  options \code{verbose} or \code{warn} to be non-zero via the
  \code{\link{options}} function.

  There is a short discussion of these additional arguments with some
  example code available at
  \url{https://www.stat.ucdavis.edu/~duncan/R/dynload/}.
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
  the specified DLL to the executing \R image.  Calls to \code{.C},
  \code{.Call}, \code{.Fortran} and \code{.External} can then be used to
  execute compiled C functions or Fortran subroutines contained in the
  library.  The return value of \code{dyn.load} is an object of class
  \code{DLLInfo}.  See \code{\link{getLoadedDLLs}} for information about
  this class.

  The function \code{dyn.unload} unlinks the DLL.  Note that unloading a
  DLL and then re-loading a DLL of the same name may or may not work: on
  Solaris it used the first version loaded. Note also that some DLLs cannot
  be safely unloaded at all: unloading a DLL which implements C finalizers
  but does not unregister them on unload causes R to crash.

  \code{is.loaded} checks if the symbol name is loaded \emph{and
  searchable} and hence available for use as a character string value
  for argument \code{.NAME} in \code{.C}, \code{.Fortran},
  \code{.Call}, or \code{.External}.  It will succeed if any one of the
  four calling functions would succeed in using the entry point unless
  \code{type} is specified.  (See \code{\link{.Fortran}} for how Fortran
  symbols are mapped.)  Note that symbols in base packages are not
  searchable, and other packages can be so marked.
}
\note{
  \code{is.loaded} requires the name you would give to \code{.C} etc.
  It must be a character string and so cannot be an \R object as used
  for registered native symbols (see \dQuote{Writing R Extensions}
  section 5.4.).  Some registered symbols are available by name but most are
  not, including those in the examples below.

  By default, the maximum number of DLLs that can be loaded is now 614
  when the OS limit on the number of open files allows or can be
  increased, but less otherwise (but it will be at least 100).  A
  specific maximum can be requested \emph{via} the environment variable
  \env{R_MAX_NUM_DLLS}, which has to be set (to a value between 100 and
  1000 inclusive) before starting an \R{} session.  If the OS limit on
  the number of open files does not allow using this maximum and cannot
  be increased, \R will fail to start with an error.  The maximum is not
  allowed to be greater than 60\% of the OS limit on the number of open
  files (essentially unlimited on Windows, on Unix typically 1024, but
  256 on macOS).  The limit can sometimes (including on macOS) be
  modified using command \command{ulimit -n} (\command{sh},
  \command{bash}) or \command{limit descriptors} (\command{csh}) in the
  shell used to launch \R{}.  Increasing \env{R_MAX_NUM_DLLS} comes with
  some memory overhead, and be aware that many types of
  \link{connections} also use file descriptors.

  If the OS limit on the number of open files cannot be determined, the
  DLL limit is 100 and cannot be changed \emph{via} \env{R_MAX_NUM_DLLS}.

  The creation of DLLs and the runtime linking of them into executing
  programs is very platform dependent.  In recent years there has been
  some simplification in the process because the C subroutine call
  \code{dlopen} has become the POSIX standard for doing this.  Under
  Unix-alikes \code{dyn.load} uses the \code{dlopen} mechanism and
  should work on all platforms which support it.  On Windows it uses the
  standard mechanism (\code{LoadLibrary}) for loading DLLs.

  The original code for loading DLLs in Unix-alikes was provided by
  Heiner Schwarte.
}
#ifdef windows
\section{Windows}{
  The \sQuote{standard mechanisms for loading DLLs} include a
  search order for where a DLL is found (if not given as an absolute
  path, which is preferred), \emph{and} of where its dependent DLLs will
  be found.  This search path depends on the version of Windows and its
  security settings, but for versions since Windows XP SP1 it is
  \itemize{
    \item The directory from which the application was launched.
    \item The various system directories,
    e.g.\sspace{}\file{c:/Windows/system32}, \file{c:/Windows/system} and
    \file{c:/Windows}.
    \item The current directory.
    \item Along the search path for executables given by the environment
    variable \env{PATH}.
  }

  Packages often want to supply dependent DLLs in their \file{libs}
  directory, and do this by setting the \env{PATH} variable
  (\code{\link{library.dynam}} does that automatically in recent
  versions of \R), but the DLL search order means that DLLs in the
  launch directory and in system directories will be preferred.  On
  Windows XP SP1 and later there is a way to modify the search order.
  If argument \code{DLLpath} is supplied to \code{dyn.load}, the latter
  makes use of the Windows system call \code{SetDllDirectory} to insert
  the value of \code{DLLpath} in second place, and removes the current
  directory, for the duration of that \code{dyn.load} call.  (Note that
  only one directory can be inserted in this way.)

  Users have been confused by messages like
  \preformatted{
    error: unable to load shared object
    '.../library/rJava/libs/x64/rJava.dll':
    LoadLibrary failure:  The specified module could not be found.
  }
  The final line is a Windows (not \R) diagnostic: the \sQuote{module}
  that could not be found is not \file{rJava.dll} but something else
  Windows is looking for (here most likely Java DLLs): if you are lucky
  there will be a dialog box with more details.
}
#endif
\section{Warning}{
  Do not use \code{dyn.unload} on a DLL loaded by
  \code{\link{library.dynam}}: use \code{\link{library.dynam.unload}}.
  This is needed for system housekeeping.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{library.dynam}} to be used inside a package's
  \code{\link{.onLoad}} initialization.

  \code{\link{SHLIB}} for how to create suitable DLLs.

  \code{\link{.C}},
  \code{\link{.Fortran}},
  \code{\link{.External}},
  \code{\link{.Call}}.
}
\examples{
## expect all of these to be false in R >= 3.0.0 as these can only be
## used via registered symbols.
is.loaded("supsmu") # Fortran entry point in stats
is.loaded("supsmu", "stats", "Fortran")
is.loaded("PDF", type = "External") # pdf() device in grDevices
}
\keyword{interface}
