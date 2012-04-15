% File src/library/base/man/ns-load.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{ns-load}
\title{Loading and Unloading Namespaces}

\alias{attachNamespace}
\alias{loadNamespace}
\alias{requireNamespace}
\alias{loadedNamespaces}
\alias{unloadNamespace}

\description{
  Functions to load and unload namespaces.
}
\usage{
attachNamespace(ns, pos = 2, dataPath = NULL, depends = NULL)
loadNamespace(package, lib.loc = NULL,
              keep.source = getOption("keep.source.pkgs"),
              partial = FALSE)
requireNamespace(package, ..., quietly = FALSE)
loadedNamespaces()
unloadNamespace(ns)
}
\arguments{
  \item{ns}{string or namespace object.}
  \item{pos}{integer specifying position to attach.}
  \item{dataPath}{path to directory containing a database of datasets to
    be lazy-loaded into the attached environment.}
  \item{depends}{\code{NULL} or a character vector of dependencies to be
    recorded in object \code{.Depends} in the package.}
  \item{package}{string naming the package/namespace to load.}
  \item{lib.loc}{character vector specifying library search path.}
  \item{keep.source}{Now ignored except during package installation.
    For more details see this argument to \code{\link{library}}.
  }
  \item{partial}{logical; if true, stop just after loading code.}
  \item{quietly}{logical: should progress and error messages be suppressed?}
  \item{\dots}{further arguments to be passed to \code{loadNamespace}.}
}
\details{
  The functions \code{loadNamespace} and \code{attachNamespace} are
  usually called implicitly when \code{library} is used to load a name
  space and any imports needed.  However it may be useful to call
  these functions directly at times.

  \code{loadNamespace} loads the specified namespace and registers it in
  an internal data base.  A request to load a namespace when one of that
  name is already loaded has no effect.  The arguments have the same
  meaning as the corresponding arguments to \code{\link{library}}, whose
  help page explains the details of how a particular installed package
  comes to be chosen.  After loading, \code{loadNamespace} looks for a
  hook function named \code{\link{.onLoad}} as an internal variable in
  the namespace (it should not be exported).  This function is called
  with the same arguments as \code{.First.lib}.  Partial loading is used
  to support installation with the \option{--lazy} option.

  \code{loadNamespace} does not attach the namespace it loads to the
  search path.  \code{attachNamespace} can be used to attach a frame
  containing the exported values of a namespace to the search path (but
  this is almost always done \emph{via} \code{\link{library}}).  The
  hook function \code{\link{.onAttach}} is run after the namespace
  exports are attached.

  \code{requireNamespace} is a wrapper for \code{loadNamespace}
  analogous to \code{\link{require}} that returns a logical value.

  \code{loadedNamespaces} returns a character vector of the names of
  the loaded namespaces.

  \code{unloadNamespace} can be used to attempt to force a namespace to
  be unloaded.  If the namespace is attached, it is first
  \code{\link{detach}}ed, thereby running a \code{.Last.lib} function
  in the namespace if one is exported.  Then an error is signaled if the
  namespace is imported by other loaded namespaces, and the namespace
  is not unloaded.  If defined, a hook function \code{\link{.onUnload}}
  is run before removing the namespace from the internal registry.

  See the comments in the help for \code{\link{detach}} about some
  issues with unloading and reloading namespaces.
}
\value{
  \code{attachNamespace} returns invisibly the package environment it
  adds to the search path.
  
  \code{loadNamespace} returns the namespace environment, either one
  already loaded or the one the function causes to be loaded.
  
  \code{requireNamespace} returns \code{TRUE} if it succeeds or
  \code{FALSE}.
  
  \code{loadedNamespaces} returns a character vector.

  \code{unloadNamespace} returns \code{NULL}, invisibly.
}
\keyword{data}
\keyword{utilities}
\author{Luke Tierney and R-core}
