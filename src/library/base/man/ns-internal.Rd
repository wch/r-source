\name{ns-internals}
\title{Name Space Internals}

\alias{asNamespace}
\alias{getNamespaceInfo}
\alias{importIntoEnv}
\alias{isBaseNamespace}
\alias{isNamespace}
\alias{namespaceExport}
\alias{namespaceImport}
\alias{namespaceImportFrom}
\alias{packageHasNamespace}
\alias{parseNamespaceFile}
\alias{registerS3method}
\alias{setNamespaceInfo}
\alias{.__S3MethodsTable__.}
\usage{
asNamespace(ns, base.OK = TRUE)
getNamespaceInfo(ns, which)
importIntoEnv(impenv, impnames, expenv, expnames)
isBaseNamespace(ns)
namespaceExport(ns, vars)
namespaceImport(self, \dots)
namespaceImportFrom(self, ns, vars)
packageHasNamespace(package, package.lib)
parseNamespaceFile(package, package.lib, mustExist = TRUE)
registerS3method(genname, class, method, envir = parent.frame())
setNamespaceInfo(ns, which, val)
}

\arguments{
  \item{ns}{string or name space environment.}
  \item{base.OK}{logical.}
  \item{impenv}{environment.}
  \item{expenv}{name space environment.}
  \item{vars}{character vector.}
  \item{self}{name space environment.}
  \item{package}{string naming the package/name space to load.}
  \item{package.lib}{character vector specifying library.}
  \item{mustExist}{logical.}
  \item{genname}{character.}
  \item{class}{character.}
  \item{envir}{environment.}
  \item{which}{character.}
  \item{val}{any object.}
  \item{\dots}{character arguments.}
}
\description{
  Internal name space support functions.  Not intended to be called directly.
}
\keyword{internal}
\author{Luke Tierney}
