\name{show}
\alias{show}
\title{Show an Object}
\usage{
show(object, printTo = stdout(), oldMethods = TRUE)
}
\description{
  display the object, by printing, plotting or whatever suits its class.

This function exists to be specialized by methods; the definition stored here
is intended as the default method.  

  In S-Plus, but not currently in R, \code{show}
is called for automatic display of the result of an evaluated expression.

}
\arguments{
\item{object}{Any R object}
\item{printTo}{Either a file or \link{connection}, or else
    \code{FALSE}.  In the latter case, the lines of text that would
    have been printed are returned as the value of the call (in a
    character vector with one element per line of output).
  }
  \item{oldMethods}{Should old-style print methods be used for this
    object?  \code{TRUE} by default if called directly, but \code{FALSE}
    when called from the methods package version of \code{print.default}.
}
}
\value{ Usually an invisible \code{NULL}, but if \code{printTo} is
  \code{FALSE}, a character vector containing the lines that would
  otherwise have been printed.
}
\details{
  The argument \code{printTo} allowing redirection is an extension to
  the API definition (and to the S-Plus function).  It applies to the
  default version of \code{show} and to the methods provided for class
  definitions and method lists (see below).  This is a trial; if it
  turns out to be useful, we will define a standard mechanism for
  honoring non-printing in user-written methods.  Meantime, if you
  want to write such methods, see the code for \code{show} for one way
  to do it.
}
\seealso{ \code{\link{showMethods}} prints all the methods for one or
  more functions;  \code{\link{showMlist}} prints individual methods lists;
  \code{\link{showClass}} prints class definitions.  Neither of
  the latter two normally needs to be called directly.
}
\keyword{programming}
