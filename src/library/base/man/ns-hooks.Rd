\name{ns-hooks}
\alias{.onLoad}
\alias{.onUnload}
\alias{.onAttach}
\title{Hooks for Name Space events}
\description{
  Packages with name spaces can supply functions to be called when
  loaded, attached or unloaded.
}

\usage{
.onLoad(libname, pkgname)
.onAttach(libname, pkgname)
.onUnload(libpath)
}
\arguments{
  \item{libname}{a character string giving the library directory where
    the package defining the namespace was found.}
  \item{pkgname}{a character string giving the name of the package,
    including the version number if the package was installed with
    \code{--with-package-versions}.}
  \item{libpath}{a character string giving the complete path to the package.}
}
\details{
  These functions apply only to packages with name spaces.

  After loading, \code{\link{loadNamespace}} looks for a hook function named
  \code{.onLoad} and runs it before sealing the namespace and processing
  exports.

  If a name space is unloaded (via \code{\link{unloadNamespace}}),
  a hook function \code{.onUnload} is run before final unloading.
  
  Note that the code in \code{.onLoad} and \code{.onUnload} is run
  without the package being on the search path, but (unless circumvented)
  lexical scope will make objects in the namespace and its imports
  visible. (Do not use the double colon operator in \code{.onLoad} as
  exports have not been processed at the point it is run.)

  When the package is attached (via \code{\link{library}}), the hook
  function \code{.onAttach} is called after the exported functions are
  attached.  This is less likely to be useful than \code{.onLoad}, which
  should be seen as the analogue of \code{\link{.First.lib}} (which is only
  used for packages without a name space).

  \code{.onLoad}, \code{.onUnload} and \code{.onAttach} are looked for
  as internal variables in the name space and should not be exported.
  
  If a function \code{\link{.Last.lib}} is visible in the package, it
  will be called when the package is detached: this does need to be exported.

  Anything needed for the functioning of the name space should be
  handled at load/unload times by the \code{.onLoad} and
  \code{.onUnload} hooks.  For example, shared libraries can be loaded
  (unless done by a \code{useDynib} directive in the \code{NAMESPACE}
  file) and initialized in \code{.onLoad} and unloaded in
  \code{.onUnload}.  Use \code{.onAttach} only for actions that are
  needed only when the package becomes visible to the user, for example
  a start-up message.

  If a package was installed with \code{--with-package-versions}, the
  \code{pkgname} supplied will be something like \code{tree_1.0-16}.
}
\seealso{
  \code{\link{setHook}} shows how users can set hooks on the same events.
}
\keyword{utilities}
