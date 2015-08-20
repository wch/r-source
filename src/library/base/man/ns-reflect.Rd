% File src/library/base/man/ns-reflect.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007, 2015 R Core Team
% Distributed under GPL 2 or later

\name{ns-reflect.Rd}
\title{Namespace Reflection Support}

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
  \item{ns}{string or namespace object.}
  \item{name}{string or name.}
}
\description{
  Internal functions to support reflection on namespace objects.
}
\seealso{
  \code{\link{loadNamespace}} for more about namespaces.
}
\details{
  \code{getExportedValue} returns the value of the exported variable
  \code{name} in namespace \code{ns}.

  \code{getNamespace} returns the environment representing the name
  space \code{name}.  The namespace is loaded if necessary.

  \code{getNamespaceExports} returns a character vector of the names
  exported by \code{ns}.

  \code{getNamespaceImports} returns a representation of the imports
  used by namespace \code{ns}.  This representation is experimental
  and subject to change.

  \code{getNamespaceName} and \code{getNamespaceVersion} return the name
  and version of the namespace \code{ns}.

  \code{getNamespaceUsers} returns a character vector of the names of
  the namespaces that import namespace \code{ns}.
}
\keyword{internal}
\author{Luke Tierney}
