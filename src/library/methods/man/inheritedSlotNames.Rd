\name{inheritedSlotNames}
\alias{inheritedSlotNames}
\title{Names of Slots Inherited From a Super Class}
\description{
  For a class (or class definition, see \code{\link{getClass}} and
  the description of class \code{\linkS4class{classRepresentation}}),
  give the names which are inherited from \dQuote{above}, i.e., super
  classes, rather than by this class' definition itself.
}
\usage{
inheritedSlotNames(Class, where = topenv(parent.frame()))
}
\arguments{
  \item{Class}{character string or
    \code{\linkS4class{classRepresentation}}, i.e., resulting from
    \code{\link{getClass}}.}
  \item{where}{environment, to be passed further to
    \code{\link{isClass}} and \code{\link{getClass}}.}
}
% \details{
% }
\value{
character vector of slot names, or \code{\link{NULL}}.
}
% \author{Martin Maechler}

\seealso{
  \code{\link{slotNames}}, \code{\link{slot}}, \code{\link{setClass}}, etc.
}
\examples{
.srch <- search()
library(stats4)
inheritedSlotNames("mle")

%% this is not run currently, as it makes later examples fail:
\dontrun{
if(require("Matrix")) {
  print( inheritedSlotNames("Matrix") ) # NULL
  ## whereas
  print( inheritedSlotNames("sparseMatrix") ) # --> Dim & Dimnames
  ##  i.e. inherited from "Matrix" class

  print( cl <- getClass("dgCMatrix") ) # six slots, etc

  print( inheritedSlotNames(cl) ) # *all* six slots are inherited
}
% currently, don't do this when running all examples: classes remain
% cached anyway :
## detach package we've attached above:
for(n in rev(which(is.na(match(search(), .srch)))))
    detach(pos = n)
}% end{dontrun}
}
\keyword{classes}
\keyword{methods}
