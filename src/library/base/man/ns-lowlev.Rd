\name{ns-lowlev}
\title{Low Level Name Space Support Functions}

\alias{attachNamespace}
\alias{loadNamespace}
\alias{loadedNamespaces}
\alias{unloadNamespace}
\alias{.onLoad}
\alias{.onUnload}
\alias{.onAttach}
\alias{loadingNamespaceInfo}
\alias{saveNamespaceImage}

\usage{
attachNamespace(ns, pos = 2)
loadNamespace(package, lib.loc = NULL,
              keep.source = getOption("keep.source.pkgs"),
              partial = FALSE, declarativeOnly = FALSE)
loadedNamespaces()
unloadNamespace(ns)
loadingNamespaceInfo()
saveNamespaceImage(package, rdafile, lib.loc = NULL,
                   keep.source = getOption("keep.source.pkgs"))
}
\arguments{
  \item{ns}{string or namespace object.}
  \item{pos}{integer specifying position to attach.}
  \item{package}{string naming the package/name space to load.}
  \item{lib.loc}{character vector specifying library search path.}
  \item{keep.source}{logical specifying whether to retain source.}
  \item{partial}{logical; if true, stop just after laoding code.}
  \item{declarativeOnly}{logical; disables \code{.Import}, etc, if true.}
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
  an internal data base.  A request to load a name space that is already
  loaded has no effect.  The arguments have the same meaning as the
  corresponding arguments to \code{library}.  After loading,
  \code{loadNamespace} looks for a hook function named \code{.onLoad} as 
  an internal variable in the name space (it should not be exported).
  This function is called with the same arguments as
  \code{.First.lib}.  Partial loading is used so support installation with
  the \option{--save} option.

  \code{loadNamespace} does not attach the name space it loads to the
  search path.  \code{attachNamespace} can be used to attach a frame
  containing the exported values of a name space to the search path.
  The hook function \code{.onAttach} is run after the name space
  exports are attached, but this is not likely to be useful.  Shared
  library loading and setting of options should be handled at load
  time by the \code{.onLoad} hook.

  \code{loadedNamespaces} returns a character vector of the names of
  the loaded name spaces.

  \code{unloadNamespace} can be used to force
  a name space to be unloaded.  An error is signaled if the name space
  is imported by other loaded name spaces.  If defined, a hook
  function \code{.onUnload}, analogous to \code{.Last.lib}, is run
  before removing the name space from the internal registry.
  \code{unloadNamespace} will first \code{\link{detach}} a package of
  the same name if one is on the path, thereby running a
  \code{.Last.lib} function in the package if one is exported.

  \code{loadingNamespaceInfo} returns a list of the arguments that
  would be passed to \code{.onLoad} when a name space is being loaded.
  An error is signaled of a name space is not currently being loaded.

  \code{saveNamespaceImage} is used to save name space images for
  packages installed with \option{--save}.
}
\keyword{internal}
\author{Luke Tierney}
