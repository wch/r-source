\name{promptClass}
\alias{promptClass}
\title{Generate a Shell for Documentation of a Formal Class}
\description{
  Assembles all relevant slot and method information for a class, with
  minimal markup for Rd processing; no QC facilities at present.
}
\usage{
promptClass(clName, filename, type = "class", keywords = "classes", where)
}
\arguments{
  \item{clName}{ character string naming the class to be documented. }
  \item{filename}{ Usually, the filename on which the documentation
    shell should be written.  By default it is the topic name for
    the class documentation, followed by \code{".Rd"}.  See the
    example below.  The argument can also be any writable
    \link{connection}.}
  \item{type}{The documentation type to be declared in the output
    file.}
  \item{keywords}{The keywords to include in the shell of the
    documentation.  The keyword \code{"classes"} should be one of
    them.  }
  \item{where}{ where to look for the definition of the class and of
    methods that use it:  by default, anywhere in the current search
    list.  }
}
\details{ 
  The class definition is found on the search list.  Using that
  definition, information about classes extended and slots is
  determined.

  In addition, the currently available generics with methods for this
  class are found (using \code{\link{getGenerics}}).  Note that these
  methods need not be in the same environment as the class definition; in
  particular, this part of the output may depend on which packages are
  currently in the search list.

  As with other prompt-style functions, the documentation shell is
  written to a file, which will need editing to give information about
  the \emph{meaning} of the class.  The output of \code{promptClass} can
  only contain information from the metadata about the formal definition
  and how it is used.
}
\value{
  The name of the file to which the shell is written (the value is
  \code{\link{invisible}}).
  A message is also printed notifying the user about the file.
}
\author{
  VJ Carey \email{stvjc@channing.harvard.edu} and John Chambers
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\seealso{\code{\link{prompt}} for documentation of functions, 
  \code{\link{promptMethods}} for documentation of method definitions.

  For processing of the edited documentation, either use \code{R CMD
    \link{Rdconv}}, or include the edited file in the \file{man}
  subdirectory of a package.
}
\examples{
\testonly{
## from setClass
## A simple class with two slots
setClass("track",
         representation(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
setClass("trackCurve",
         representation("track", smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         representation(x="numeric", y="matrix", smooth="matrix"),
         prototype = list(x=numeric(), y=matrix(0,0,0), smooth= matrix(0,0,0)))

setIs("trackMultiCurve", "trackCurve",
  test = function(obj) {ncol(slot(obj, "y")) == 1},
  coerce = function(obj) { new("trackCurve", x = slot(obj, "x"),
        y = as.numeric(slot(obj,"y")), smooth = as.numeric(slot(obj, "smooth")))})

## from setMethod

setMethod("plot", "track",
 function(x, y, ...) plot(slot(x, "y"), y,  ...)
)
setMethod("plot", c("trackCurve", "missing"),
function(x, y, ...) {
  plot(as(x, "track"))
  if(length(slot(x, "smooth") > 0))
    lines(slot(x, "x"), slot(x, "smooth"))
  }
)

promptClass("trackMultiCurve", stdout())

promptClass("track", stdout())
}
\dontrun{> promptClass("track")
A shell of class documentation has been written to the 
file "track-class.Rd".
}}

\keyword{programming}
\keyword{classes}
