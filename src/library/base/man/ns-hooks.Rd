% File src/library/base/man/ns-hooks.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{ns-hooks}
\alias{.onLoad}
\alias{.onUnload}
\alias{.onAttach}
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
.Last.lib(libpath)
}
\arguments{
  \item{libname}{a character string giving the library directory where
    the package defining the namespace was found.}
  \item{pkgname}{a character string giving the name of the package.}
  \item{libpath}{a character string giving the complete path to the package.}
}
\details{
  After loading, \code{\link{loadNamespace}} looks for a hook function named
  \code{.onLoad} and runs it before sealing the namespace and processing
  exports.

  If a function \code{\link{.Last.lib}} is visible in the package, it
  will be called when the package is detached: this does need to be
  exported.  Beware that it might be called if \code{.onAttach} or
  \code{.onLoad} has failed, so it should be written defensively.  (It
  is called within \code{\link{try}}, so errors will not stop the
  package being detached.)
  
  If a namespace is unloaded (via \code{\link{unloadNamespace}}),
  a hook function \code{.onUnload} is run before final unloading.
  
  Note that the code in \code{.onLoad} and \code{.onUnload} is run
  without the package being on the search path, but (unless circumvented)
  lexical scope will make objects in the namespace and its imports
  visible.  (Do not use the double colon operator in \code{.onLoad} as
  exports have not been processed at the point it is run.)

  When the package is attached (via \code{\link{library}} or
  \code{\link{attachNamespace}}), the hook function \code{.onAttach} is
  looked for and if found is called after the exported functions are
  attached and before the package environment is sealed.

  \code{.onLoad}, \code{.onUnload} and \code{.onAttach} are looked for
  as internal variables in the namespace and should not be exported
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
\seealso{
  \code{\link{setHook}} shows how users can set hooks on the same events, and 
  lists the sequence of events involving all of the hooks.
}
\keyword{utilities}
