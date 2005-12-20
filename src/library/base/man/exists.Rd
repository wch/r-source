\name{exists}
\alias{exists}
\title{Is an Object Defined?}
\description{
  Look for an \R object of the given name.
}
\usage{
exists(x, where = -1, envir = , frame, mode = "any",
       inherits = TRUE)
}
\arguments{
  \item{x}{a variable name (given as a character string).}
  \item{where}{where to look for the object (see the details section); if
    omitted, the function will search as if the name of the object
    appeared unquoted in an expression.}
  \item{envir}{an alternative way to specify an environment to look in,
    but it's usually simpler to just use the \code{where} argument.}
  \item{frame}{a frame in the calling list.  Equivalent to giving
    \code{where} as \code{sys.frame(frame)}.}
  \item{mode}{the mode of object sought.  For the meaning, see the
    Details section of the help for \code{\link{exists}}.}
  \item{inherits}{should the enclosing frames of the environment be
    searched?}
}
\details{
  The \code{where} argument can specify the environment in which to look
  for the object in any of several ways: as an integer (the position in
  the \code{\link{search}} list); as the character string name of an
  element in the search list; or as an \code{\link{environment}}
  (including using \code{\link{sys.frame}} to access the currently active
  function calls).  The \code{envir} argument is an alternative way to
  specify an environment, but is primarily there for back compatibility.

  This function looks to see if the name \code{x} has a value bound to
  it in the specified environment. If \code{inherits} is \code{TRUE} and
  a value is not found for \code{x} in the specified environment, the
  enclosures of the environment are searched until the name \code{x}
  is encountered.  See \code{\link{environment}} and the \sQuote{R
  Language Definition} manual for details about the structure of
  environments and their enclosures.
 
  \bold{Warning:}
  \code{inherits=TRUE} is the default behaviour for \R but not for S.

  If \code{mode} is specified then only objects of that mode are sought.
  The \code{mode} may specify collections such as \code{"numeric"} and
  \code{"function"}: any member of the collection will suffice.
}
\value{
  Logical, true if and only if an object of the correct name and mode is
  found.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{get}}.
}
\examples{
##  Define a substitute function if necessary:
if(!exists("some.fun", mode="function"))
  some.fun <- function(x) { cat("some.fun(x)\n"); x }
search()
exists("ls", 2) # true even though ls is in pos=3
exists("ls", 2, inherits = FALSE) # false
}
\keyword{data}
