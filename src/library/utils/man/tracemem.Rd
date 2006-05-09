\name{tracemem}
\alias{tracemem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trace copying of objects}
\description{
This function marks an object so that a message is printed whenever the
internal function \code{duplicate} is called.   This happens when two
objects share the same memory  and one of them is modified. It is a
major cause of hard-to-predict memory use in R.  
}
\usage{
tracemem(x)
untracemem(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An R object, not a function or environment or \code{NULL}}
}
\details{
This functionality is optional, determined at compilation, because it
makes R run a little  more slowly even when no objects are being traced.
  
When an object is traced any duplication of the object produces a
message to standard output. The message consists of the string
\code{memtrace}, the identifying strings for the object being copied and
the new object being created, and a stack trace showing where the
duplication occurred.

The messages can be turned off with \code{\link{tracingState}}.

It is not possible to trace functions, as this would conflict with
\code{\link{trace}} and it is not useful to trace \code{NULL},
environments, promises, weak references, or external pointer objects, as
these are not duplicated.
}
\value{
A string for identifying the object in the trace output.
}

\seealso{\code{\link{trace}}
  
  \code{\link{memRprof}}

  \url{http://developer.r-project.org/memory-profiling.html}
}
\examples{
\dontrun{
a<-1:10
tracemem(a)
## b and a share memory
b<-a
b[1]<-1

## copying in lm
d<-rnorm(10)
tracemem(d)
lm(d~a+log(b))

## this doesn't use duplicate and so is not traced
f<-b+1


}


}
\keyword{utilities}% at least one, from doc/KEYWORDS

