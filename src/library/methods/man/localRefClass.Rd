% File src/library/methods/man/refClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2010-2013 R Core Team
% Distributed under GPL 2 or later

\name{LocalReferenceClasses}
\title{Localized Objects based on Reference Classes}
\alias{LocalReferenceClasses}
\alias{localRefClass-class}
\alias{$<-,localRefClass-method} %$
\description{
Local reference classes are modified \link{ReferenceClasses} that
isolate the objects to the local frame.  Therefore, they do \emph{not}
propagate changes back to the calling environment.   At the same time,
they use the reference field semantics locally, avoiding the automatic
duplication applied to standard \R objects.

The current implementation has no special construction.  To create a
local reference class, call \code{\link{setRefClass}()} with a
\code{contains=} argument that includes \code{"localRefClass"}.  See
the example below.

Local reference classes operate essentially as do regular, functional
classes in \R; that is, changes are made by assignment and take place
in the local frame.
The essential difference is that replacement operations (like the
change to the \code{twiddle} field in the example) do not cause
duplication of the entire object, as would be the case for a formal
class or for data with attributes or in a named list.
The purpose is to allow large objects in some fields that are not
changed along with potentially frequent changes to other fields, but
without copying the large fields.

}
\usage{
\special{setRefClass(Class, fields = , contains = c("localRefClass",....),
     methods =, where =, ...)}
}

\details{
Localization of objects is only partially automated in the current implementation.
Replacement expressions using the \code{$<-} operator are safe. %$

However, if reference methods for the class themselves modify fields,
using \code{<<-}, for example, then 
one must ensure that the object is local to the relevant frame before
any such method is called.
Otherwise, standard reference class behavior still prevails.

There are two ways to ensure locality.  The direct way is to invoke
the special
method \code{x$ensureLocal()} on the object.
The other way is to modify a field explicitly by \code{x$field <- ...}
It's
only necessary that  one or the other of these happens
once for each object, in order to trigger the shallow copy that
provides locality for the references.  In the example below, we show
both mechanisms.

However it's done, localization must occur
\emph{before} any methods make changes.  (Eventually, some use of code
tools should at least largely automate this process, although it may
be difficult to guarantee success under arbitrary circumstances.)
}

\author{
  John Chambers
}

\examples{
## class "myIter" has a BigData field for the real (big) data
## and a "twiddle" field for some parameters that it twiddles
## ( for some reason)

myIter <- setRefClass("myIter", contains = "localRefClass",
  fields = list(BigData = "numeric", twiddle = "numeric"))

tw <- rnorm(3)
x1 <- myIter(BigData = rnorm(1000), twiddle = tw) # OK, not REALLY big

twiddler <- function(x, n) {
  x$ensureLocal() # see the Details.  Not really needed in this example
  for(i in seq(length = n)) {
      x$twiddle <- x$twiddle + rnorm(length(x$twiddle))
      ## then do something ....
      ## Snooping in gdb, etc will show that x$BigData is not copied
  }
  return(x)
}

x2 <- twiddler(x1, 10)

stopifnot(identical(x1$twiddle, tw), !identical(x1$twiddle, x2$twiddle))

}
\keyword{ programming }
\keyword{ classes }
