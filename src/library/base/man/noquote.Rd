\name{noquote}
\alias{noquote}
\alias{print.noquote}
\alias{as.matrix.noquote}
\alias{c.noquote}
\alias{[.noquote}
\title{Class for ``no quote'' Printing of Character Strings}
\description{
  Print character strings without quotes.
}
\usage{
noquote(obj)
\method{print}{noquote}(x, \dots)
\method{c}{noquote}(\dots, recursive = FALSE)
}
\arguments{
  \item{obj}{any \R object, typically a vector of
    \code{\link{character}} strings.}
  \item{x}{an object of class \code{"noquote"}.}
  \item{\dots}{further options passed to next methods, such as \code{\link{print}}.}
  \item{recursive}{for compatibility with the generic \code{\link{c}} function.}
}
\details{
  \code{noquote} returns its argument as an object of class
  \code{"noquote"}.  There is a method for \code{c()} and subscript
  method (\code{"[.noquote"}) which ensures that the class is not lost
  by subsetting.  The print method (\code{print.noquote}) prints
  character strings \emph{without} quotes (\code{"\dots"}).

  These functions exist both as utilities and as an example of using (S3)
  \code{\link[base]{class}} and object orientation.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{
  \code{\link{methods}},
  \code{\link[base]{class}},
  \code{\link{print}}.
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
