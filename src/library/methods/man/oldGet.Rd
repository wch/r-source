% File src/library/methods/man/oldGet.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{oldGet}
\alias{getAccess}
\alias{getClassName}
\alias{getClassPackage}
\alias{getExtends}
\alias{getProperties}
\alias{getPrototype}
\alias{getSubclasses}
\alias{getVirtual}
\title{Old functions to access slots in a class definition }
\description{
 \bold{These functions are deprecated.}

 They do nothing but access a slot in a class definition, and don't
 even do this consistently  with the right name (they date back to the
 early implementation of the \pkg{methods} package).
 Higher-level functions for the useful operations (e.g.,
 \code{\link{extends}} for \code{getExtends}) should be used instead.

}
\usage{
getAccess(ClassDef)

getClassName(ClassDef)

getClassPackage(ClassDef)

getExtends(ClassDef)

getProperties(ClassDef)

getPrototype(ClassDef)

getSubclasses(ClassDef)

getVirtual(ClassDef)

}
\arguments{
  \item{ClassDef}{ the class definition object }
}
\details{
  The functions should be replaced by direct access to the slots, or
  by use of higher-level alternatives.

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

}
\seealso{Class \code{\linkS4class{classRepresentation}}.}
\keyword{internal}
