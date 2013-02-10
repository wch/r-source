% File src/library/base/man/ns-hooks.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{ns-hooks}
\alias{.onLoad}
\alias{.onUnload}
\alias{.onAttach}
\alias{.onDetach}
\alias{.Last.lib}
\title{Hooks for Namespace Events}
\description{
  Packages can supply functions to be called when
  loaded, attached, detached or unloaded.
}

\usage{
.onLoad(libname, pkgname)
.onAttach(libname, pkgname)
.onUnload(libpath)
.onDetach(libpath)
.Last.lib(libpath)
}
\arguments{
  \item{libname}{a character string giving the library directory where
    the package defining the namespace was found.}
  \item{pkgname}{a character string giving the name of the package.}
  \item{libpath}{a character string giving the complete path to the package.}
}
\details{
  After loading, \code{\link{loadNamespace}} looks for a hook function
  named \code{.onLoad} and calls it (with two unnamed arguments) before
  sealing the namespace and processing exports.

  When the package is attached (via \code{\link{library}} or
  \code{\link{attachNamespace}}), the hook function \code{.onAttach} is
  looked for and if found is called (with two unnamed arguments) before
  the package environment is sealed.

  If a function \code{.onDetach} (as from \R 3.0.0) is in the namespace
  or \code{.Last.lib} is exported from the package, it will be called
  (with a single argument) when the package is detached.  Beware that it
  might be called if \code{.onAttach} has failed, so it should be
  written defensively.  (It is called within \code{\link{tryCatch}}, so
  errors will not stop the package being detached.)

  If a namespace is unloaded (via \code{\link{unloadNamespace}}), a hook
  function \code{.onUnload} is run (with a single argument) before final
  unloading.

  Note that the code in \code{.onLoad} and \code{.onUnload} should not
  assume any package except the base package is on the search path.
  Objects in the current package will be visible (unless this is
  circumvented), but objects from other packages should be imported or
  the double colon operator should be used.
  
  \code{.onLoad}, \code{.onUnload} and \code{.onAttach} are looked for
  as internal objects in the namespace and should not be exported
  (whereas \code{.Last.lib} should be).

  Anything needed for the functioning of the namespace should be
  handled at load/unload times by the \code{.onLoad} and
  \code{.onUnload} hooks.  For example, DLLs can be loaded (unless done
  by a \code{useDynLib} directive in the \file{NAMESPACE} file) and
  initialized in \code{.onLoad} and unloaded in \code{.onUnload}.  Use
  \code{.onAttach} only for actions that are needed only when the
  package becomes visible to the user (for example a start-up message)
  or need to be run after the package environment has been created.
}

\section{Good practice}{
  Loading a namespace should where possible be silent, with startup
  messages given by \code{.onAttach}.  These messages (and any essential
  ones from \code{.onLoad}) should use \code{\link{packageStartupMessage}}
  so they can be silenced where they would be a distraction.

  There should be no calls to \code{library} nor \code{require} in these
  hooks.  The way for a package to load other packages is via the
  \samp{Depends} field in the \file{DESCRIPTION} file: this ensures
  that the dependence is documented and packages are loaded in the
  correct order.  Loading a namespace should not change the search path,
  so rather than attach a package, dependence of a namespace on another
  package should be achieved by (selectively) importing from the other
  package's namespace.

  Uses of \code{library} with argument \code{help} to display basic
  information about the package should use \code{format} on the
  computed package information object and pass this to
  \code{packageStartupMessage}.

  There should be no calls to \code{\link{installed.packages}} in startup
  code: it is potentially very slow and may fail in versions of \R
  before 2.14.2 if package installation is going on in parallel.  See
  its help page for alternatives.

  Compiled code should be loaded (e.g. \emph{via}
  \code{\link{library.dynam}}) in \code{.onLoad} or a \code{useDynLib}
  directive in the \file{NAMESPACE} file, and not in \code{.onAttach}.
  Similarly, compiled code should not be unloaded (e.g. \emph{via}
  \code{\link{library.dynam.unload}}) in \code{.Last.lib} nor
  \code{.onDetach}, only in \code{.onUnload}.
}

\seealso{
  \code{\link{setHook}} shows how users can set hooks on the same events, and
  lists the sequence of events involving all of the hooks.
}
\keyword{utilities}
