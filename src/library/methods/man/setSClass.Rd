\name{setSClass}
\alias{setSClass}
\title{ Low-level Class definition }
\description{
 This is the lower-level version of \code{\link{setClass}}.  Unless you
 really know why you are calling it, user \code{setClass} instead.
}
\usage{
setSClass(name, properties=list(), extends=character(),
   prototype=NULL, generatorFunction, where=1,
   subclasses=character(), virtual=NA)
}

\arguments{
  \item{name}{ character string name for the class }
  \item{properties}{ the \code{representation} argument to \code{setClass} }
  \item{extends}{ what classes does this class extend }
  \item{prototype}{ an object (usually a list) providing the default
    data for the slots specified in the representation. }
  \item{generatorFunction}{ an optional function to use in generating
    new objects from the class }
  \item{where}{ where to store the resulting definition (as metadata).
    By default, uses the global environment. }
  \item{subclasses}{ Classes that extend \emph{this} class.  }
  \item{virtual}{ Is this known to be a virtual class? }
}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\seealso{ \code{\link{setClass}} }
\keyword{programming}
\keyword{classes}

