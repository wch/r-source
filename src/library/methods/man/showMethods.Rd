\name{showMethods}
\alias{showMethods}
\title{ Show all the methods for the specified function(s)}
\description{
  Show a summary of the methods for one or more generic functions,
  possibly restricted to those involving specified classes.
}
\usage{
showMethods(f=character(), where=-1, classes=NULL, includeDefs=FALSE, 
    inherited=TRUE, printTo = stdout())
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{f}{ One or more function names.  If omitted, all the functions
    on database \code{where} will be examined.
    }
  \item{where}{If \code{where} is supplied, the methods definition from that database will be used; otherwise,
the current definition is used (which will include inherited methods that have arisen so
far in the session).  }
  \item{classes}{ If argument \code{classes} is supplied, it is a vector of class names
that restricts the displayed
results to those methods whose signatures include one or more of those
classes. }
  \item{includeDefs}{ If \code{includeDefs} is \code{TRUE}, include the definitions of the
    individual methods in the printout. }
  \item{inherited}{ If \code{inherits} is \code{TRUE}, then methods that have been found by inheritance, so
far in the session, will be included and marked as inherited. }
\item{printTo}{The connection on which the printed information will be
    written.  If \code{printTo} is \code{FALSE}, the output will be
    collected as a character vector and returned as the value of the
    call to \code{showMethod}.   See \code{\link{show}}.
    }
}
\details{
 The output style is different from S-Plus in that it does not show the database
from which the definition comes, but can optionally include the method definitions.

}
\value{
  If \code{printTo} is \code{FALSE}, the character vector that would
  have been printed is returned; otherwise the value is the connection or filename. 
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
\seealso{\code{\link{setMethod}}, and \code{\link{GenericFunctions}}
  for other tools involving methods; \code{\link{show}} for }
\examples{
\testonly{example(setMethod)}
## assuming the generic and methods for plotData
## are set up as in the documentation for setMethod,
## print (with definitions) the methods that involve
## class "track"
showMethods("plotData", classes = "track")

\keyword{methods}
