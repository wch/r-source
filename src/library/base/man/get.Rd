\name{get}
\alias{get}
\title{Return a Variable's Value}
\description{
  Search for an \R object with a given name and return it if found.
}
 
\usage{
get(x, pos=-1, envir=as.environment(pos), mode="any", inherits=TRUE)
}
\arguments{
  \item{x}{a variable name (given as a character string).}
  \item{pos}{where to look for the object (see the details section); if
    omitted, the function will search, as if the name of the object
    appeared in unquoted in an expression.
  }
  \item{envir}{an alternative way to specify an environment to look in;
    see the details section.}
  \item{mode}{the mode of object sought.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
}
\details{
  The \code{pos} argument can specify the  environment in which to look
  for the object in any of several ways:
  as an integer (the position in the \code{\link{search}} list); as
  the character string name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}} to
  access the currently active function calls).
  The \code{envir} argument is an alternative way to specify an
  environment, but is primarily there for back compatibility.
  
   The \code{mode} includes collections such as \code{"numeric"} and
   \code{"function"}: any member of the collection will suffice. 
}
\value{
  This function searches the specified environment for a bound variable
  whose name is given by the character string \code{x}.  If the
  variable's value is not of the correct \code{mode}, it is ignored.

  If \code{inherits} is \code{FALSE}, only the first frame of the
  specified environment is inspected.  If \code{inherits} is
  \code{TRUE}, the search is continued up through the parent frames
  until a bound value of the right mode is found.

  Using a \code{NULL} environment is equivalent to using the current
  environment.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{exists}}.
}
\examples{
get("\%o\%")
}
\keyword{data}
