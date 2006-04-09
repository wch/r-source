\name{getClass}
\alias{getClass}
\alias{getClassDef}
\title{Get Class Definition }
\description{
  Get the definition of a class.
}
\usage{
getClass(Class, .Force = FALSE, where)
getClassDef(Class, where, package)
}
\arguments{
  \item{Class}{ the character-string name of the class.}
  \item{.Force}{ if \code{TRUE}, return \code{NULL} if the class is
    undefined; otherwise, an undefined class results in an error.}
  \item{where}{ environment from which to begin the search for the definition; by default,
    start at the top-level (global) environment and proceed through
    the search list.}
  \item{package}{ the name of the package asserted to hold the
      definition.  Supplied instead of \code{where}, with the
      distinction that the package need not be currently attached.}
}
\details{
  A call to \code{getClass} returns the complete definition of the class
  supplied as a string, including all slots, etc. in classes that this
  class extends.  A call to \code{getClassDef} returns the definition of
  the class from the environment \code{where}, unadorned.  It's usually \code{getClass}
  you want.

  If you really want to know whether a class is formally
  defined, call \code{\link{isClass}}.
}
\value{
  The object defining the class.  This is an object of class
  \code{"classRepEnvironment"}.  However, \emph{do not} deal with the
  contents of the object directly unless you are very sure you know what
  you're doing.  Even then, it is nearly always better practice to use
  functions such as \code{\link{setClass}} and \code{\link{setIs}}.
  Messing up a class object will cause great confusion.
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the methods package follows the
  reference, the R software is an original implementation, so details in
  the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\seealso{
  \link{Classes},
  \code{\link{setClass}},
  \code{\link{isClass}}.
}
\examples{
getClass("numeric") ## a built in class

cld <- getClass("thisIsAnUndefinedClass", .Force = TRUE)
cld ## a NULL prototype
## If you are really curious:
str(cld)
## Whereas these generate errors:
try(getClass("thisIsAnUndefinedClass"))
try(getClassDef("thisIsAnUndefinedClass"))
}
\keyword{programming}
\keyword{classes}
