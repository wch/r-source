\name{showMethods}
\alias{showMethods}
\title{ Show all the methods for the specified function(s)}
\description{
  Show a summary of the methods for one or more generic functions,
  possibly restricted to those involving specified classes.
}
\usage{
showMethods(f = character(), where = topenv(parent.frame()),
            classes = NULL, includeDefs = FALSE, inherited = TRUE,
            printTo = stdout())
}
\arguments{
  \item{f}{one or more function names.  If omitted, all functions
    will be examined. }
  \item{where}{If \code{where} is supplied, the methods definition from
    that position will be used; otherwise, the current definition is
    used (which will include inherited methods that have arisen so far
    in the session). If \code{f} is omitted, \code{where} controls
    where to look for generic functions.}
  \item{classes}{ If argument \code{classes} is supplied, it is a vector
    of class names that restricts the displayed results to those methods
    whose signatures include one or more of those classes. }
  \item{includeDefs}{ If \code{includeDefs} is \code{TRUE}, include the
    definitions of the individual methods in the printout. }
  \item{inherited}{ If \code{inherits} is \code{TRUE}, then methods that
    have been found by inheritance, so far in the session, will be
    included and marked as inherited. Note that an inherited method
    will not usually appear until it has been used in this session.
    See \code{\link{selectMethod}} if you want to know what method is
    dispatched for particular classes of arguments.}
  \item{printTo}{The connection on which the printed information will be
    written.  If \code{printTo} is \code{FALSE}, the output will be
    collected as a character vector and returned as the value of the
    call to \code{showMethod}.   See \code{\link{show}}.
  }
}
\details{
  The output style is different from S-Plus in that it does not show the
  database from which the definition comes, but can optionally include
  the method definitions.
}
\value{
  If \code{printTo} is \code{FALSE}, the character vector that would
  have been printed is returned; otherwise the value is the connection
  or filename.
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
\seealso{
  \code{\link{setMethod}}, and \code{\link{GenericFunctions}}
  for other tools involving methods;
  \code{\link{selectMethod}} will show you the method dispatched for a
  particular function and signature of classes for the arguments.
}
\examples{
\dontshow{
 setClass("track",
          representation(x="numeric", y="numeric"))
 ## First, with only one object as argument:
 setMethod("plot", signature(x="track", y="missing"),
           function(x,  y, ...) plot(slot(x, "x"), slot(x, "y"), ...))
 ## Second, plot the data from the track on the y-axis against anything
 ## as the x data.
 setMethod("plot", signature(y = "track"),
           function(x, y, ...) plot(x, slot(y, "y"), ...))
 setMethod("plot", "track",
           function(x, y, ...) plot(slot(x, "y"), y,  ...))
}
## Assuming the methods for plot
## are set up as in the example of help(setMethod),
## print (without definitions) the methods that involve class "track":
showMethods("plot", classes = "track")
\dontrun{
Function "plot":
x = ANY, y = track
x = track, y = missing
x = track, y = ANY}
}
\keyword{methods}
