\name{REnvironment}
\alias{assignEnv}
\alias{assignGlobal}
\alias{removeGlobal}
\alias{getEnv}
\alias{existsEnv}
\alias{objectsEnv}
\alias{removeEnv}
\alias{copyEnvironment}
\alias{defaultMetaEnvironment}
\title{Utilities for Using an Environment as a Database}
\usage{
assignEnv(name, value, environment)

assignGlobal(name, value)

removeGlobal(name)

getEnv(name, environment)

existsEnv(name, environment)

removeEnv(names, environment)

objectsEnv(ev, all.names=FALSE, pattern)

copyEnvironment(object)

defaultMetaEnvironment()
}
\description{
  These are utility functions for the class/method package.  They should not be used directly.
  In particular, code that uses them will not port to S-Plus.

  The functions in this group, except for copyEnvironment and defaultMetaEnvironment, are shortcuts
  to invoke the corresponding general functions with an explicit
  environment as an argument.

  \code{copyEnvironment}:
  Returns a new environment in which all the objects found in the object's
  original environment have been assigned.  The environment may be NULL, in
  which case \code{NULL} is returned.
  
  \code{defaultMetaEnvironment}:
  the environment in which metadata for classes and methods should be stored,
 if the `where' argument is not specified.

 Normally, this is the global environment.  But during
 package loading, it is the corresponding package environment.

 Eventually, the core code for package loading should ensure this; for now, we
 rely on the source for the individual package setting
 options(packageEnvironment).
  See the example below
  
}
\author{
  John Chambers
}
\examples{
\dontrun{
## put this code at the beginning of your package's source if you
## need to set any class or methods in your package, called "MyPkg"
## in this example.
options(packageEnvironment = 
   as.environment(match("package:MyPkg", search())))

## Now you can source in calls to setClass, setMethod, setGeneric
## and other functions that define classes and methods.
##  ......

##
## Now, BE SURE to end with this code, so further setClass, etc use
## the global environment by default:
options(packageEnvironment = NULL)

}

}
\keyword{programming}
\keyword{classes}
\keyword{methods}
