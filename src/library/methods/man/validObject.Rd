\name{validObject}
\alias{validObject}
\alias{setValidity}
\alias{getValidity}
\title{ Test the Validity of an Object }
\description{
  The validity of \code{object} related to its class definition is
  tested.  If the object is valid, \code{TRUE} is returned; otherwise,
  either a vector of strings describing validity failures is returned,
  or an error is generated (according to whether \code{test} is
  \code{TRUE}).

  The functions \code{getValidity} and \code{setValidity} get and set
  the validity method of a class.  This method is a function of one
  object that returns \code{TRUE} or a description of the non-validity.
}
\usage{
validObject(object, test)

getValidity(ClassDef)
setValidity(ClassDef, method)
}
\arguments{
  \item{object}{ Any object, but not much will happen unless the
    object's class has a formal definition.}
  \item{test}{ If \code{test} is \code{TRUE}, and validity fails the
    function returns a vector of strings describing the problems.  If
    \code{test} is \code{FALSE} (the default) validity failure generates
    an error.}
  \item{ClassDef}{The name of the class whose validity method is to be
    set.}
  \item{method}{A validity method;  that is, either \code{NULL} or a
    function of one argument (the \code{object}).  Like
    \code{validObject}, the function should return \code{TRUE} if the
    object is valid, and one or more descriptive strings if any problems
    are found.  Unlike \code{validObject}, it should never generate an
    error.

    Note that validity methods do not have to check validity of any
    slots or superclasses:  the logic of \code{validObject} ensures
    these tests are done once only.  As a consequence, if one validity
    method wants to use another, it should extract and call the method
    from the other definition of the other class by calling
    \code{\link{getValidity}}:  it should \emph{not} call
    \code{validObject}.}
}
\details{
  Validity testing takes place ``bottom up'':  first the validity of the
  object's slots, if any, is tested.  Then for each of the classes that
  this class extends (the ``superclasses''), the explicit validity
  method of that class is called, if one exists.  Finally, the validity
  method of \code{object}'s class is called, if there is one.

  Testing generally stops at the first stage of finding an error, except
  that all the slots will be examined even if a slot has failed its
  validity test.
}
\value{
  \code{validObject} returns \code{TRUE} if the object is valid.
  Otherwise a vector of strings describing problems found, except that
  if \code{test} is \code{FALSE}, validity failure generates an error,
  with the corresponding strings in the error message.
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html}
  is the primary documentation.

  The functions in this package emulate the facility for classes
  and methods described in \emph{Programming with Data} (John
  M. Chambers, Springer, 1998).  See this book for further details and
  examples.
}
\author{
  John Chambers
}

\seealso{ \code{\link{setClass}}. }

\examples{
  setClass("track",
    representation(x="numeric", y = "numeric"))
  t1 <- new("track", x=1:10, y=sort(rnorm(10)))
  ## A valid "track" object has the same number of x, y values
  validTrackObject <- function(x){
    if(length(x@x) == length(x@y)) TRUE
    else paste("Unequal x,y lengths: ", length(x@x), ", ", length(x@y),
    sep="")
  }
  ## assign the function as the validity method for the class
  setValidity("track", validTrackObject)
  ## t1 should be a valid "track" object
  validObject(t1)
  ## Now we do something bad
  t1@x <- 1:20
  ## This should generate an error
  try(validObject(t1))
}

\keyword{programming}
\keyword{classes}
