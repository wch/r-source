\name{getClass}
\alias{getClass}
\alias{getClassDef}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  Get the definition of a class.
}
\usage{
getClass(Class)
getClassDef(Class, where=-1)
}
\arguments{
  \item{Class}{ the character-string name of the class.}

  \item{where} { where to search for the definition; by default,
    anywhere on the current search list.}
}
\details{
  
  A call to \code{getClass} returns the complete definition of the class supplied as a string,
including all slots, etc. in classes that this class extends. A call to
\code{getClassDef} returns  the definition of the class from that
database, unadorned.  It's usually \code{getClass} you want.

However, \code{getClass} never fails; if the argument is undefined as a
class, it returns a default definition for a virtual class.  If you want
to fail should the definition not exist, use \code{getClassDef}
(although \code{\link{isClass}} may be a better way to do the same thing).


  
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

\seealso{ \link{Classes}, \code{\link{setClass}}. }

\examples{
getClass("numeric") ## a built in class

getClass("thisIsAnUndefinedClass") ## returns a trivial definition
try(getClassDef("thisIsAnUndefinedClass")) ## generates an error
}
\keyword{programming}
\keyword{classes}
