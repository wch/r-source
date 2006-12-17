\name{tracemem}
\alias{tracemem}
\alias{untracemem}
\alias{retracemem}
\title{Trace Copying of Objects}
\description{
This function marks an object so that a message is printed whenever the
internal function \code{duplicate} is called.   This happens when two
objects share the same memory  and one of them is modified.  It is a
major cause of hard-to-predict memory use in R.  
}
\usage{
tracemem(x)
untracemem(x)
retracemem(x, previous = NULL)
}
\arguments{
  \item{x}{An R object, not a function or environment or \code{NULL}.}
  \item{previous}{A value as returned by \code{tracemem} or \code{retracemem}.}
}
\details{
  This functionality is optional, determined at compilation, because it
  makes R run a little more slowly even when no objects are being
  traced. \code{tracemem} and \code{untracemem} give errors when R is not
  compiled with memory profiling; \code{retracemem} does not (so it can be
  left in code during development).
  
  When an object is traced any copying of the object by the C function
  \code{duplicate} or by arithmetic or mathmetical operations produces a
  message to standard output.  The message consists of the string
  \code{tracemem}, the identifying strings for the object being copied and
  the new object being created, and a stack trace showing where the
  duplication occurred.  \code{retracemem()} is used to indicate that a
  variable should be considered a copy of a previous variable (e.g. after
  subscripting).

  The messages can be turned off with \code{\link{tracingState}}.

  It is not possible to trace functions, as this would conflict with
  \code{\link{trace}} and it is not useful to trace \code{NULL},
  environments, promises, weak references, or external pointer objects, as
  these are not duplicated.
}
\value{
  A character string for identifying the object in the trace output (an
  address in hex enclosed in angle brackets), or \code{NULL} (invisibly
  for \code{untracemem}.
}

\seealso{
  \code{\link{trace}}, \code{\link{Rprofmem}}

  \url{http://developer.r-project.org/memory-profiling.html}
}
\examples{\dontrun{
a <- 1:10
tracemem(a)
## b and a share memory
b <- a
b[1] <- 1
untracemem(a)

## copying in lm
d <- rnorm(10)
tracemem(d)
lm(d ~ a+log(b))

## f is not a copy and is not traced
f <- d[-1]
f+1
## indicate that f should be traced as a copy of d
retracemem(f, retracemem(d))
f+1
}}
\keyword{utilities}

