\name{getClass}
\alias{getClass}
\alias{getClassDef}
\title{Get Class Definition }
\description{
  Get the definition of a class.
}
\usage{
getClass(Class, .Force = FALSE)
getClassDef(Class, where=-1)
}
\arguments{
  \item{Class}{ the character-string name of the class.}

  \item{.Force}{ if \code{TRUE}, return \code{NULL} if the class is
    undefined; otherwise, an undefined class results in an error.
    }

  \item{where} { where to search for the definition; by default,
    anywhere on the current search list.}
}
\details{
  
  A call to \code{getClass} returns the complete definition of the class supplied as a string,
including all slots, etc. in classes that this class extends. A call to
\code{getClassDef} returns  the definition of the class from that
database, unadorned.  It's usually \code{getClass} you want.

The statement that \code{getClass} fails if the class is undefined is
not quite true at the moment.  A few special vector types and informally
defined classes are allowed, and \code{getClass} returns \code{NULL} for
these. But don't count on it; eventually these holes in the class system
should be plugged.

In any case, if you really want to know whether a class is formally
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
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}

\seealso{ \link{Classes}, \code{\link{setClass}}, \code{\link{isClass}}. }

\examples{
getClass("numeric") ## a built in class

\testonly{
getClass("thisIsAnUndefinedClass", .Force = TRUE) ## returns NULL
try(getClass("thisIsAnUndefinedClass")) ## generates an error
try(getClassDef("thisIsAnUndefinedClass")) ## generates an error
}
}
\keyword{programming}
\keyword{classes}
