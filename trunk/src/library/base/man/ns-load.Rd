\name{ns-load}
\title{Loading and Unloading Name Spaces}

\alias{attachNamespace}
\alias{loadNamespace}
\alias{loadedNamespaces}
\alias{unloadNamespace}

\description{
  Functions to load and unload namespaces.
}
\usage{
attachNamespace(ns, pos = 2, dataPath = NULL)
loadNamespace(package, lib.loc = NULL,
              keep.source = getOption("keep.source.pkgs"),
              partial = FALSE, declarativeOnly = FALSE)
loadedNamespaces()
unloadNamespace(ns)
}
\arguments{
  \item{ns}{string or namespace object.}
  \item{pos}{integer specifying position to attach.}
  \item{dataPath}{path to directory containing a database of datasets to
    be lazy-loaded into the attached environment.}
  \item{package}{string naming the package/name space to load.}
  \item{lib.loc}{character vector specifying library search path.}
  \item{keep.source}{
    logical specifying whether to retain source.
    This applies only to the specified name space, and not to other name spaces
    which might be loaded to satisfy imports.

    For more details see this argument to \code{\link{library}}.
  }
  \item{partial}{logical; if true, stop just after loading code.}
  \item{declarativeOnly}{logical; disables \code{.Import}, etc, if true.}
}
\details{
  The functions \code{loadNamespace} and \code{attachNamespace} are
  usually called implicitly when \code{library} is used to load a name
  space and any imports needed.  However it may be useful to call
  these functions directly at times.

  \code{loadNamespace} loads the specified name space and registers it in
  an internal data base.  A request to load a name space that is already
  loaded has no effect.  The arguments have the same meaning as the
  corresponding arguments to \code{\link{library}}, whose help page
  explains the details of how a particular installed package comes to be
  chosen.  After loading,
  \code{loadNamespace} looks for a hook function named
  \code{\link{.onLoad}} as an internal variable in the name space (it
  should not be exported).  This function is called with the same arguments
  as \code{.First.lib}.  Partial loading is used to support installation with
  the \option{--save} and \option{--lazy} options.

  \code{loadNamespace} does not attach the name space it loads to the
  search path.  \code{attachNamespace} can be used to attach a frame
  containing the exported values of a name space to the search path.
  The hook function \code{\link{.onAttach}} is run after the name space
  exports are attached.

  \code{loadedNamespaces} returns a character vector of the names of
  the loaded name spaces.

  \code{unloadNamespace} can be used to force
  a name space to be unloaded.  An error is signaled if the name space
  is imported by other loaded name spaces.  If defined, a hook
  function \code{\link{.onUnload}} is run
  before removing the name space from the internal registry.
  \code{unloadNamespace} will first \code{\link{detach}} a package of
  the same name if one is on the path, thereby running a
  \code{.Last.lib} function in the package if one is exported.
}
\keyword{data}
\keyword{utilities}
\author{Luke Tierney}
