\name{get}
\alias{get}
\title{Return a Variable's Value}
\description{
  Search for an \R object with a given name and return it if found.
}
 
\usage{
get(x, pos=-1, envir=pos.to.env(pos), mode="any", inherits=TRUE)
}
\arguments{
  \item{x}{a variable name (given as a quoted character string).}
  \item{pos}{position in search list, can be numerical or a quoted
    string.}
  \item{envir}{the environment to be used.}
  \item{mode}{the mode of object sought.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
}
\details{
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
\seealso{
  \code{\link{exists}}.
}
\examples{
get("\%o\%")
}
\keyword{data}
