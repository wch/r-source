\name{noquote}
\title{Class for ``no quote'' Printing of Strings}
\usage{
noquote(obj)
print.noquote(x, \dots)
obj[j]
}
\alias{noquote}
%> ./Extract.Rd for "[" and "[<-"
\alias{print.noquote}
\alias{as.matrix.noquote}
\arguments{
 \item{obj, x}{any \R object; typically a vector of \code{\link{character}}
   strings.}
 \item{\dots}{further options for \code{\link{print}}.}
}
\description{
  These functions exist both as utilities and as an example of using
  \code{\link{class}} and object orientation.
}
\value{
  \code{noquote} returns its argument as an object of class
  \code{"noquote"}.  The function \code{"[.noquote"} ensures that the
  class is not lost by subsetting.

  For (default) printing, \code{print.noquote} will be used which prints
  characters \emph{without} quotes (\code{"\dots"}).
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{\code{\link{methods}},\code{\link{class}},\code{\link{print}}.
}
\examples{
letters
nql <- noquote(letters)
nql
nql[1:4] <- "oh"
nql[1:12]

cmp.logical <- function(log.v)
{
  ## Purpose: compact printing of logicals
  log.v <- as.logical(log.v)
  noquote(if(length(log.v)==0)"()" else c(".","|")[1+log.v])
}
cmp.logical(runif(20) > 0.8)
}
\keyword{print}
\keyword{methods}
\keyword{utilities}
