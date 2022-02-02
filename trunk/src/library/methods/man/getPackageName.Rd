% File src/library/methods/man/getPackageName.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{getPackageName}
\alias{getPackageName}
\alias{setPackageName}
\alias{packageSlot}
\alias{packageSlot<-}
\title{The Name associated with a Given Package}
\description{
  The functions below produce the package associated with a particular
  environment or position on the search list, or of the package
  containing a particular function.  They are primarily used to support
  computations that need to differentiate objects on multiple packages.
}
\usage{
getPackageName(where, create = TRUE)
setPackageName(pkg, env)

packageSlot(object)
packageSlot(object) <- value
}
\arguments{
  \item{where}{the environment or position on the search list
    associated with the desired package.}
  \item{object}{object providing a character string name, plus the
    package in which this object is to be found.}
  \item{value}{the name of the package.}
   \item{create}{flag, should a package name be created if none can be
     inferred?  If \code{TRUE} and no non-empty package name is found,
     the current date and time are used as a package name, and a
     warning is issued. The created name is stored in the environment
     if that environment is not locked.}
  \item{pkg, env}{make the string in \code{pkg} the internal
    package name for all computations that set class and method
    definitions in environment \code{env}.}
}
\details{
  Package names are normally installed during loading of the package,
  by the \link{INSTALL} script or by the \code{\link{library}}
  function.  (Currently, the name is stored as the object
  \code{.packageName} but don't trust this for the future.)
}
\value{
  \code{getPackageName} returns the character-string name of the package
  (without the extraneous \code{"package:"} found in the search list).

  \code{packageSlot} returns or sets the package name slot (currently
  an attribute, not a formal slot, but this may change someday).

  \code{setPackageName} can be used to establish a package name in an
  environment that would otherwise not have one.  This
  allows you to create classes and/or methods in an arbitrary
  environment, but it is usually preferable to create packages by the
  standard \R programming tools (\code{\link{package.skeleton}}, etc.)
}

\seealso{ \code{\link{search}}, \code{\link{packageName}} }

\examples{
## all the following usually return "base"
getPackageName(length(search()))
getPackageName(baseenv())
getPackageName(asNamespace("base"))
getPackageName("package:base")

}
\keyword{ programming }% at least one, from doc/KEYWORDS
