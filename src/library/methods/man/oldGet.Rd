\name{oldGet}
\alias{getAccess}
\alias{getClassName}
\alias{getClassPackage}
\alias{getExtends}
\alias{getProperties}
\alias{getPrototype}
\alias{getSubclasses}
\alias{getValidity}
\alias{getVirtual}
\title{Old functions to access slots in a class definition }
\description{
Expect these functions to become deprecated in the near future.

 They do nothing but access a slot in a class definition, and don't
 even do this consistently  with the right name (they date back to the
 early implementation of the methods package).
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

getValidity(ClassDef) 

getVirtual(ClassDef) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ClassDef}{ the class definition object }
}
\details{
  The functions should be replaced by direct access to the slots.
The functions and corresponding slots are: 
\code{getAccess}, slot \code{"access"};
\code{getClassName}, slot \code{"className"};
\code{getClassPackage}, slot \code{"package"};
\code{getExtends}, slot \code{"contains"};
\code{getProperties}, slot \code{"slots"};
\code{getPrototype}, slot \code{"prototype"};
\code{getSubclasses}, slot \code{"subclasses"};
\code{getValidity}, slot \code{"validity"};
\code{getVirtual}, slot \code{"virtual"}.

}
\seealso{ \code{\link{classRepresentation-class}} }
\keyword{internal}
