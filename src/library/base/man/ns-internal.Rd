% File src/library/base/man/ns-internal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{ns-internals}
\title{Namespace Internals}

\alias{asNamespace}
\alias{getNamespaceInfo}
\alias{.getNamespaceInfo}
\alias{importIntoEnv}
\alias{isBaseNamespace}
\alias{isNamespace}
\alias{namespaceExport}
\alias{namespaceImport}
\alias{namespaceImportClasses}
\alias{namespaceImportFrom}
\alias{namespaceImportMethods}
\alias{packageHasNamespace}
\alias{parseNamespaceFile}
\alias{registerS3method}
\alias{registerS3methods}
\alias{setNamespaceInfo}
\alias{.__S3MethodsTable__.}
\alias{.mergeExportMethods}
\alias{.mergeImportMethods}
\alias{.knownS3Generics}
\alias{loadingNamespaceInfo}
\alias{.getNamespace}
\alias{.getNamespaceInfo}
\alias{..getNamespace}
\usage{
asNamespace(ns, base.OK = TRUE)
getNamespaceInfo (ns, which)
.getNamespaceInfo(ns, which)
importIntoEnv(impenv, impnames, expenv, expnames)
isBaseNamespace(ns)
isNamespace(ns)
namespaceExport(ns, vars)
namespaceImport(self, \dots, from = NULL)
namespaceImportFrom(self, ns, vars, generics, packages,
                    from = "non-package environment")
namespaceImportClasses(self, ns, vars, from = NULL)
namespaceImportMethods(self, ns, vars, from = NULL)
packageHasNamespace(package, package.lib)
parseNamespaceFile(package, package.lib, mustExist = TRUE)
registerS3method(genname, class, method, envir = parent.frame())
registerS3methods(info, package, env)
setNamespaceInfo(ns, which, val)
.mergeExportMethods(new, ns)
.mergeImportMethods(impenv, expenv, metaname)
.knownS3Generics
loadingNamespaceInfo()
.getNamespace(name)
..getNamespace(name, where)
}

\arguments{
  \item{ns}{string or namespace environment.}
  \item{base.OK}{logical.}
  \item{impenv}{environment.}
  \item{expenv}{namespace environment.}
  \item{vars}{character vector.}
  \item{generics}{optional character vector.}
  \item{self}{namespace environment.}
  \item{package}{string naming the package/namespace to load.}
  \item{packages}{vector of package names parallel to \code{generics}.}
  \item{package.lib}{character vector specifying library.}
  \item{mustExist}{logical.}
  \item{genname}{character.}
  \item{class}{character.}
  \item{envir, env}{environment.}
  \item{info}{a 3-column character matrix.}
  \item{which}{character.}
  \item{val}{any object.}
  \item{\dots}{character arguments.}
  \item{metaname}{the methods table name.}
  \item{name}{symbol: name of namespace}
}
\description{
  Internal namespace support functions.  Not intended to be called
  directly, and only visible because of the special nature of the
  \pkg{base} namespace.
}
\details{
  \code{packageHasNamespace} does not indicate if the package has a
  namespace (all now do), rather if it has a \file{NAMESPACE} file,
  which \pkg{base} and some legacy packages do not.  But then you are
  not intended to be using it \dots.
}
\seealso{
  \code{\link{loadNamespace}} or \code{\link{getNamespace}} are somewhat
  higher level namespace related functions.
}
\author{
  Luke Tierney and other members of the R Core Team.
}
\examples{
nsName <- "stats"
(ns <- asNamespace(nsName)) # <environment: namespace:stats>

## Inverse function of asNamespace() :
environmentName(asNamespace("stats")) # "stats"
environmentName(asNamespace("base"))  # "base"
getNamespaceInfo(ns, "spec")[["name"]] ## -> "stats"
\dontshow{stopifnot(identical(nsName,
                      getNamespaceInfo(ns, "spec")[["name"]]))}

## Only for for the daring ones, trying to get into the bowels :

lsNamespaceInfo <- function(ns, ...) {
    ns <- asNamespace(ns, base.OK = FALSE)
    ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE))
}
allinfoNS <- function(ns) sapply(lsNamespaceInfo(ns), getNamespaceInfo, ns=ns)

utils::str(allinfoNS("stats"))
utils::str(allinfoNS("stats4"))
}
\keyword{internal}
