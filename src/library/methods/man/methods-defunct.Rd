% File src/library/methods/man/methods-defunct.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{methods-defunct}
\alias{methods-defunct}
\alias{getAllMethods}
\alias{mlistMetaName}
\alias{removeMethodsObject}
\alias{getAccess}
\alias{getClassName}
\alias{getClassPackage}
\alias{getExtends}
\alias{getProperties}
\alias{getPrototype}
\alias{getSubclasses}
\alias{getVirtual}
\title{Defunct Functions in the Methods Package}
\description{
  Defunct functions in package \pkg{methods}.
}
\usage{
% Removed in 2.8.0
getAccess(ClassDef)
getClassName(ClassDef)
getClassPackage(ClassDef)
getExtends(ClassDef)
getProperties(ClassDef)
getPrototype(ClassDef)
getSubclasses(ClassDef)
getVirtual(ClassDef)

getAllMethods(f, fdef, where)
mlistMetaName (name, package)
removeMethodsObject(f, where)
% </Removed 2.8.0>
}
\details{
  The first block of functions should be replaced by direct access to
  the slots, or by use of higher-level alternatives.

  The functions and corresponding slots are:
  \tabular{ll}{
    \code{getAccess} \tab \code{"access"} \cr
    \code{getClassName} \tab \code{"className"} \cr
    \code{getClassPackage} \tab \code{"package"} \cr
    \code{getExtends} \tab \code{"contains"} \cr
    \code{getProperties} \tab \code{"slots"} \cr
    \code{getPrototype} \tab \code{"prototype"} \cr
    \code{getSubclasses} \tab \code{"subclasses"} \cr
    \code{getVirtual} \tab \code{"virtual"}
  }

  \code{getAllMethods} replicated the behavior of
  \code{\link{getMethods}}, which should be called instead to obtain
  approximately the old behavior of \code{getAllMethods}.  However, a
  better substitution is to use \code{\link{findMethods}}, which
  returns a simpler structure.

  \code{mlistMetaName} was a utility to  return the mangled
  name for the object that stored the \code{MethodsList}
  representation of the methods for a particular generic function,
  typically in the environment of a package.  These objects are no
  longer used.

  \code{removeMethodsObject} removed the metadata object containing
  methods for \code{f}.
}

\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
