\name{ns-lowlev}
\title{Low Level Name Space Support Functions}

\alias{attachNamespace}
\alias{loadNamespace}
\alias{loadedNamespaces}
\alias{unloadNamespace}

\usage{
attachNamespace(ns, pos = 2)
loadNamespace(package, lib.loc = NULL,
              keep.source = getOption("keep.source.pkgs"))
loadedNamespaces()
unloadNamespace(ns)
}
\arguments{
  \item{ns}{string or namespace object.}
  \item{pos}{integer specifying position to attach.}
  \item{package}{string naming the package/name space to load.}
  \item{lib.loc}{character vector specifying library search path.}
  \item{keep.source}{logical specifying whether to retain source.}
}
\description{
  Low level name space support functions.
}
\details{
  The functions \code{loadNamespace} and \code{attachNamespace} are
  usually called implicitly when \code{library} is used to load a name
  space and any imports needed.  However it may be useful to call
  these functions directly at times.

  \code{loadNamespace} loads the specified name space and registers it in
  an internal data base.  A request to load a name space that is
  already loaded has no effect.  The arguments have the same meaning
  as the corresponding arguments to \code{library}.  After loading
  \code{loadLibrary} looks for a hook function named \code{.onLoad} as
  an internal variable in the name space (it should not be exported).
  This function is called with the same arguments as
  \code{.First.lib}.

  \code{loadNamespace} does not attach the name space it loads to the
  search path.  \code{attachNamespace} can be used to attach a frame
  containing the exported values of a name space to the search path.
  The hook function \code{.onAttach} is run after the name space
  exports are attached, but this is net likely to be useful.  Shared
  library loading and setting of options should be handled at load
  time by the \code{.onLoad} hook.

  \code{loadedNamespaces} returns a character vector of the names of
  the loaded name spaces. \code{unloadNamespace} can be used to force
  a name space to be unloaded.  An error is signaled if the name space
  is imported by other loaded name spaces.  If defined, a hook
  function \code{onUnload}, analogous to \code{.Last.lib}, is run
  after removing the name space from the internal registry.
}
\keyword{internal}
\author{Luke Tierney}
