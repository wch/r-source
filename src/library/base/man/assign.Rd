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
  \item{envir}{the \code{\link{environent}} in which to assign.  The
    default is the environment where the call to \code{assign} takes place.}
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
\seealso{\code{\link{get}}, \code{\link{environment}}.}
\examples{
for(i in 1:6) { #-- Create objects  'r1', 'r2', ... 'r6' --
 nam <- paste("r",i, sep=".")
 assign(nam, 1:i)
}
ls(pat="^r..$")

##-- Global assignment within a function:
myf <- function(x) {
 innerf <- function(x) assign("Global.res", x^2, env = .GlobalEnv)
 innerf(x+1)
}
myf(3)
Global.res # 16
}
\keyword{data}
