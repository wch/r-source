\name{assign}
\title{Assign a Value to a Name}
\usage{
assign(x, value, pos = -1, envir = sys.frame(sys.parent()),
       inherits = FALSE, immediate = TRUE)
}
\alias{assign}
\arguments{
  \item{x}{a variable name (given as a quoted string).}
  \item{value}{a value to be assigned to \code{x}.}
  \item{pos}{???}
  \item{envir}{the environent in which to assign.  The default is the
    environment where the call to \code{assign} takes place.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
  \item{immediate}{an ignored compatibility feature.}
}
\value{
  This function is invoked for its side effect, which is assigning
  \code{value} to the variable \code{x}.  If no \code{envir} is
  specified, then the assignment takes place in the currently active
  environment.

  If \code{inherits} is \code{TRUE}, parents of the supplied
  environment are searched until the variable \code{x} is encountered.
  The value is then assigned in the environment in which the variable is
  encountered.  If the symbol is not encountered then assignment takes
  place in the global environment.

  If \code{inherits} is \code{FALSE}, assigment takes place in the
  initial frame of \code{envir}.
}
\keyword{data}
