\name{mode}
\title{The (Storage) Mode of an Object}
\usage{
mode(x)
mode(x) <- "<mode>"
storage.mode(x)
storage.mode(x) <- "<mode>"
}
\alias{mode}
\alias{mode<-}
\alias{storage.mode}
\alias{storage.mode<-}
\description{
  \code{mode} and \code{storage.mode} provide means of determining the
  type or storage mode of an object.  Both return a character string
  giving the (storage) mode of the object -- often the same -- both
  relying on the output of \code{\link{typeof}(x)}, see the example
  below.

  The two assignment versions are currently identical.  Both
  \code{mode(x) <- newmode} and \code{storage.mode(x) <- newmode} change
  the \code{mode} or \code{storage.mode} of object \code{x} to
  \code{newmode}.
}
\seealso{
  \code{\link{typeof}} for the R-internal ``mode'',
  \code{\link{attributes}}.
}
\examples{
sapply(options(),mode)

cex3 <- c("1","1:1","1i","list(1)","args","lm",
  "formals(lm)[[2]]","y~x","(y~x)[[1]]", "expression(x <- pi)[[1]][[1]]")
lex3 <- sapply(cex3, function(x) eval(parse(text=x)))
mex3 <- t(sapply(lex3, function(x) c(typeof(x), storage.mode(x), mode(x))))
dimnames(mex3) <- list(cex3, c("typeof(.)","mode(.)","storage.mode(.)"))
mex3

## This also makes a local copy of  `pi':
storage.mode(pi) <- "complex"
storage.mode(pi)
rm(pi)
}
\keyword{attribute}
