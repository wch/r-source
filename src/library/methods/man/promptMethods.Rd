\name{promptMethods}
\alias{promptMethods}
\title{ Generate a Shell for Documentation of Formal Methods }
\description{
  Generates a shell of documentation for the methods of a generic
  function.
}
\usage{
promptMethods(f, filename, addTo = FALSE, inherited=FALSE, type = "methods")
}
\arguments{
  \item{f}{ The name of the generic function whose methods are to be
    documented.}
  \item{filename}{ Optional file on which to write the documentation shell.
    If \code{FALSE}, the text is returned, presumably to be used in the
    documentation of the generic function itself (see
    \code{\link{prompt}}).  By default, this is the same as the coded
    topic name for these methods (currently \code{"NAME-methods.Rd"},
    where \code{NAME} is the function name, argument \code{f}).
  }
  \item{addTo}{
    Optionally, text to add to the shell.  This would typically be part
    of the contents of the documentation file for the generic function,
    if you want the generic and the methods documented together.
  }
  \item{inherited}{ Should inherited methods be included?  Default
    \code{FALSE}. }
  \item{type}{The documentation type to be declared in the output file.}
}
\value{
  If \code{filename} is \code{FALSE}, the text generated; otherwise, the
  name of the file written.
}
\seealso{
  \code{\link{prompt}} and
  \code{\link{promptClass}}
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\author{
  John Chambers
}
\keyword{programming}
\keyword{methods}
