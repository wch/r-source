\name{ns-reflect.Rd}
\title{Name Space Reflection Support}

\alias{getExportedValue}
\alias{getNamespace}
\alias{getNamespaceExports}
\alias{getNamespaceImports}
\alias{getNamespaceName}
\alias{getNamespaceUsers}
\alias{getNamespaceVersion}

\usage{
getExportedValue(ns, name)
getNamespace(name)
getNamespaceExports(ns)
getNamespaceImports(ns)
getNamespaceName(ns)
getNamespaceUsers(ns)
getNamespaceVersion(ns)
}
\arguments{
  \item{ns}{string or name space object.}
  \item{name}{string or name.}
}
\description{
  Functions to support reflection on name space objects.
}
\details{
  \code{getExportedValue} returns the value of the exported variable
  \code{name} in name space \code{ns}.

  \code{getNamespace} returns the environment representing the name
  space \code{name}.  The name space is loaded if necessary.

  \code{getNamespaceExports} returns a character vector of the names
  exported by \code{ns}.

  \code{getNamespaceImports} returns a representation of the imports
  used by name space \code{ns}.  This representation is experimental
  and subject to change.

  \code{getNamespaceName} and \code{getNamespaceVersion} return the name
  and version of the name space \code{ns}.

  \code{getNamespaceUsers} returns a character vector of the names of
  the name spaces that import name space \code{ns}.
}
\keyword{internal}
\author{Luke Tierney}
