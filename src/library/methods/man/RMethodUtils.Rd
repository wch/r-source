\name{RMethodUtils}
\alias{makeGeneric}
\alias{defaultDumpName}
\alias{checkInheritedMethod}
\alias{SessionMethodMetaData}
\alias{getFromMethodMetaData}
\alias{assignToMethodMetaData}
\alias{removeFromMethodMetaData}
\title{Utilities for Managing Method Definitions}
\usage{
makeGeneric(f, fdef)

defaultDumpName(generic, signature)

checkInheritedMethod(f, def, ev)

getFromMethodMetaData(name)

assignToMethodMetaData(name, value)

removeFromMethodMetaData(name)
}
\description{
  These are utility functions for the class/method package.  They should not be used directly.
  In particular, code that uses them will not port to S-Plus.
}
\details{
  
  \code{makeGeneric}: 
  Makes a generic function object corresponding to the given function name.

Simulates the getOrMakeGeneric S4 function but using the environment mechanism
rather than metadata as in S.

  
  \code{defaultDumpName}: 
  the default name to be used for dumping a method.

  \code{checkInheritedMethod}: 
  Examine the environment to see if this appears to be an inherited method,
by looking for the signature, ".Classes" in the environment \code{ev}.
If it is found, insert the method in the methods list for this generic.

  
  \code{SessionMethodMetaData}: 
  The name of an environment in which generic functions are stored
  during the session.
  (Should only be used through the functions documented below.)

Storing the objects there allows inherited methods to be inserted explicitly,
avoiding the cost of looking them up each time.  These assignments should not
however, go to the permanent storage, since they can change with changes in
methods or classes.

Setting a method for a generic removes the previous version of the generic
from the session metadata.

  
  \code{getFromMethodMetaData}: 
  get the version of this object stored on the meta-data for methods.
Returns \code{NULL}, rather than an error, if the object is not found.

  
  \code{assignToMethodMetaData}: 
  assign to the session metadata for methods.

  
  \code{removeFromMethodMetaData}: 
  remove from the session metadata for methods.

}
\author{
  John Chambers
}
\examples{
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
