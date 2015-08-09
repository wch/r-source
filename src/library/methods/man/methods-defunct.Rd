% File src/library/methods/man/methods-defunct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
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
\alias{seemsS4Object}
\alias{allGenerics}
\alias{trySilent}
\alias{traceOn}
\alias{traceOff}
\title{Defunct Functions in Package \pkg{methods}}
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

seemsS4Object(object)

% Removed in 2.9.0
allGenerics()

% Removed in 2.11.0
trySilent(expr)

% Removed in 3.0.0
traceOn(what, tracer=browseAll, exit=NULL)
traceOff(what)
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

  \code{seemsS4Object} was similar to \code{\link{isS4}} and superseded
  by the introduction of the S4 bit in \R 2.4.0.

  \code{allGenerics} is superseded by \code{\link{getGenerics}}.

  \code{trySilent(ex)} was a deprecated equivalent of \code{\link{try}(e,
    silent=TRUE)}; in \R programming it is typically more efficient and
  flexible to work with something like \code{\link{tryCatch}(ex,
    error = function(e) e)} instead.

  The functions \code{traceOn} and \code{traceOff} have been replaced by
  extended versions of the functions \code{\link{trace}} and
  \code{\link{untrace}}.
}

\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
