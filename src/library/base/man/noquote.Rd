\name{noquote}
\title{Class for ``no quote'' Printing of Strings}
\usage{
noquote(obj)
print.noquote(obj, \dots)
}
\alias{noquote}
\alias{[.noquote}
\alias{print.noquote}
\arguments{
 \item{obj}{any \R object; typically a vector of \code{\link{character}}
   strings.}
 \item{\dots}{further options for \code{\link{print}}.}
}
\description{
  These functions exist both as utilities and as an example of using
  \code{\link{class}} and object orientation.
}
\value{
  \code{\link{noquote}} returns its argument as an object of class
  \code{"noquote"}.
  For (default) printing, \code{print.noquote} will be used which prints
  characters \emph{without} quotes (\code{"\dots"}).
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{\code{\link{methods}},\code{\link{class}},\code{\link{print}}.
}
\examples{
letters
noquote(letters)

cmp.logical <- function(log.v)
{
  ## Purpose: compact printing of logicals
  if(!is.logical(log.v)) {
    warning("coercing argument 'log.v' to logical")
    log.v <- as.logical(log.v)
  }
  structure(if(length(log.v)==0)"()" else c(".","|")[1+log.v],
	    class = "noquote")
}
cmp.logical(runif(20) > 0.8)
}
\keyword{print}
\keyword{methods}
\keyword{utilities}

