\name{validObject}
\alias{validObject}
\alias{setValidity}
\title{ Test the Validity of an Object }
\description{
  The validity of \code{object} related to its class definition is
  tested.  If the object is valid, \code{TRUE} is returned; otherwise,
  either a vector of strings describing validity failures is returned,
  or an error is generated (according to whether \code{test} is
  \code{TRUE}).

  The function \code{setValidity} sets the validity method of a class
  (but more normally, this method will be supplied as the
  \code{validity} argument to \code{\link{setClass}}).  The method
  should be a function of one object that returns \code{TRUE} or a
  description of the non-validity.
}
\usage{
validObject(object, test = FALSE)

setValidity(Class, method, where = topenv(parent.frame()) )
}
\arguments{
  \item{object}{ Any object, but not much will happen unless the
    object's class has a formal definition.}
  \item{test}{ If \code{test} is \code{TRUE}, and validity fails the
    function returns a vector of strings describing the problems.  If
    \code{test} is \code{FALSE} (the default) validity failure generates
    an error.}
  \item{Class}{the name or class definition of the class whose validity
    method is to be set.}
  \item{method}{a validity method;  that is, either \code{NULL} or a
    function of one argument (the \code{object}).  Like
    \code{validObject}, the function should return \code{TRUE} if the
    object is valid, and one or more descriptive strings if any problems
    are found.  Unlike \code{validObject}, it should never generate an
    error.
   }
   \item{where}{the modified class definition will be stored in this
     environment.}

   Note that validity methods do not have to check validity of any
   slots or superclasses:  the logic of \code{validObject} ensures
   these tests are done once only.  As a consequence, if one validity
   method wants to use another, it should extract and call the method
   from the other definition of the other class by calling
   \code{\link{getValidity}}:  it should \emph{not} call
   \code{validObject}.
}
\details{
  Validity testing takes place \dQuote{bottom up}:  first the validity
  of the object's slots, if any, is tested.  Then for each of the
  classes that this class extends (the \dQuote{superclasses}), the
  explicit validity method of that class is called, if one exists.
  Finally, the validity method of \code{object}'s class is called, if
  there is one.

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
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
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
\dontrun{try(validObject(t1))}
\dontshow{stopifnot(is(try(validObject(t1)), "try-error"))}
}

\keyword{programming}
\keyword{classes}
