\name{promptMethods}
\alias{promptMethods}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Generate a Shell for Documentation of Formal Methods }
\description{
  Generates a shell of documentation for the methods of a generic
  function, with
}
\usage{
promptMethods(f, filename, inherited=FALSE)
}
\arguments{
  \item{f}{ The name of the generic function whose methods are to be documented.  }
  \item{filename}{ Should a file be written with the documentation shell?
    If \code{FALSE}, the text is returned, presumably to be used in the
    documentation of the generic function itself (see
    \code{\link{prompt}}). May also be the name of a file on which to
    write the documentation. By default, this is the same as the coded
    topic name for these methods (currently ]code{"methods_NAME.Rd"},
    where \code{NAMAE} is the function name, argument \code{f}).
  }
  \item{inherited}{ Should inherited methods be included?  Default \code{FALSE}. }
}
\value{
  If \code{filename} is \code{FALSE}, the text generated; otherwise, the
  name of the file written.
}
\seealso{  \code{\link{prompt}} and  \code{\link{promptClass}}  }

\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\keyword{programming}
\keyword{methods}
