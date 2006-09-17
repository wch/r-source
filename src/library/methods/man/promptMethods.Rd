\name{promptMethods}
\alias{promptMethods}
\title{ Generate a Shell for Documentation of Formal Methods }
\description{
  Generates a shell of documentation for the methods of a generic
  function.
}
\usage{
promptMethods(f, filename = NULL, methods)
}
\arguments{
  \item{f}{a character string naming the generic function whose methods
    are to be documented.}
  \item{filename}{usually, a connection or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to the coded topic name for these methods
    (currently, \code{f} followed by \code{"-methods.Rd"}).  Can also be
    \code{FALSE} or \code{NA} (see below).}
  \item{methods}{Optional methods list object giving the methods to be
    documented.  By default, the first methods object for this generic
    is used (for example, if the current global environment has some
    methods for \code{f}, these would be documented).

    If this argument is supplied, it is likely to be
    \code{getMethods(f, where)}, with \code{where} some package
    containing methods for \code{f}.}
}
\value{
  If \code{filename} is \code{FALSE}, the text generated;
  if \code{filename} is \code{NA}, a list-style representation of the
  documentation shell.
  Otherwise, the name of the file written to is returned invisibly.
}
\details{
  If \code{filename} is \code{FALSE}, the text created is returned,
  presumably to be inserted some other documentation file, such as the
  documentation of the generic function itself (see
  \code{\link{prompt}}). 

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is created and returned.  Writing the shell to a
  file amounts to \code{cat(unlist(x), file = filename, sep = "\\n")},
  where \code{x} is the list-style representation.

  Otherwise, the documentation shell is written to the file specified by
  \code{filename}.
}
\seealso{
  \code{\link{prompt}} and
  \code{\link{promptClass}}
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
}
\keyword{programming}
\keyword{methods}
