\name{promptMethods}
\alias{promptMethods}
\title{ Generate a Shell for Documentation of Formal Methods }
\description{
  Generates a shell of documentation for the methods of a generic
  function.
}
\usage{
promptMethods(f, filename, methods)
}
\arguments{
  \item{f}{ The name of the generic function whose methods are to be
    documented.}
  \item{filename}{ Optional file on which to write the documentation
    shell. If supplied, it can be the name of a file, a connection, or \code{FALSE}.
    
    If \code{FALSE}, the text is returned, presumably to be inserted
    some other documentation file, such as the documentation of the
    generic function itself (see \code{\link{prompt}}).

    The default file name is the same as the coded
    topic name for these methods (currently \code{"f-methods.Rd"}.
  }
  \item{methods}{Optional methods list object giving the methods to be
    documented. By default, the first methods object for this generic is
    used (for example, if the current global environment has some
    methods for \code{f}, these would be documented).

    If this argument is supplied, it is likely to be \code{getMethods(f,
      where)}, with \code{where} some package containing methods for \code{f}.
  }
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
  The R package \code{methods} implements, with a few exceptions, the
  programming interface for classes
  and methods in the book \emph{Programming with Data} (John
  M. Chambers, Springer, 1998), in particular sections 1.6, 2.7, 2.8,
  and chapters 7 and 8.

  While the programming interface for the methods package follows the reference,
  the R software is an original implementation, so details in
  the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page 
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\keyword{programming}
\keyword{methods}
