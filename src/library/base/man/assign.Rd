\name{assign}
\alias{assign}
\title{Assign a Value to a Name}
\description{
  Assign a value to a name in an environment.
}
\usage{
assign(x, value, pos = -1, envir = as.environment(pos),
       inherits = FALSE, immediate = TRUE)
}
\arguments{
  \item{x}{a variable name (given as a quoted string in the function call).}
  \item{value}{a value to be assigned to \code{x}.}
  \item{pos}{where to do the assignment.  By default, assigns into the
    current environment.  See the details for other possibilities.}
  \item{envir}{the \code{\link{environment}} to use. See the details section.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
  \item{immediate}{an ignored compatibility feature.}
}
\details{
  The \code{pos} argument can specify the  environment in which to assign
  the object in any of several ways:
  as an integer (the position in the \code{\link{search}} list); as
  the character string name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}} to
  access the currently active function calls).
  The \code{envir} argument is an alternative way to specify an
  environment, but is primarily there for back compatibility.

  \code{assign} does not dispatch assignment methods, so it cannot be
  used to set elements of vectors, names, attributes, etc.
}
\value{
  This function is invoked for its side effect, which is assigning
  \code{value} to the variable \code{x}.  If no \code{envir} is
  specified, then the assignment takes place in the currently active
  environment.

  If \code{inherits} is \code{TRUE}, enclosing environments of the supplied
  environment are searched until the variable \code{x} is encountered.
  The value is then assigned in the environment in which the variable is
  encountered.  If the symbol is not encountered then assignment takes
  place in the user's workspace (the global environment).

  If \code{inherits} is \code{FALSE}, assignment takes place in the
  initial frame of \code{envir}.
}

\seealso{
  \code{\link{<-}},
  \code{\link{get}},
  \code{\link{exists}},
  \code{\link{environment}}.}
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

a <- 1:4
assign("a[1]", 2)
a[1] == 2          #FALSE
get("a[1]") == 2   #TRUE

}
\keyword{data}
