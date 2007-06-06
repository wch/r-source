\name{relist}
\alias{relist}
\alias{relist.default}
\alias{relist.list}
\alias{relist.factor}
\alias{relist.matrix}
\alias{as.relistable}
\alias{is.relistable}
\alias{unlist.relistable}
%
\title{Allow Re-Listing an unlisted() Object}
\description{
  \code{relist()} is an S3 generic function with a few methods in order
  to allow easy inversion of \code{\link{unlist}(obj)} when that is used
  with an object \code{obj} of (S3) class \code{"relistable"}.
}
\usage{
relist(flesh, skeleton)
\method{relist}{default}(flesh, skeleton = attr(flesh, "skeleton"))
\method{relist}{factor}(flesh, skeleton = attr(flesh, "skeleton"))
\method{relist}{list}(flesh, skeleton = attr(flesh, "skeleton"))
\method{relist}{matrix}(flesh, skeleton = attr(flesh, "skeleton"))

as.relistable(x)
is.relistable(x)

\method{unlist}{relistable}(x, recursive = TRUE, use.names = TRUE)
}

\arguments{
  \item{flesh}{ .....}
  \item{skeleton}{ .........}
  \item{x}{an \R object, typically a list (or vector).}
  \item{recursive}{logical.  Should unlisting be applied to list
    components of \code{x}?}
  \item{use.names}{logical.  Should names be preserved?}
}
\details{
  Some functions need many parameters, which are most easily represented in
  complex structures.  Unfortunately, many mathematical functions in \R,
  including \code{\link{optim}} and \code{\link{nlm}} can only operate on
  functions whose domain is
  a vector.  \R has \code{\link{unlist}()} to convert complex objects into a
  vector representation.  \code{relist()}, it's methods and the
  functionality mentioned here provide the inverse operation to convert
  vectors back to the convenient structural representation.
  This allows structured functions (such as \code{optim()}) to have simple
  mathematical interfaces.

  For example, a likelihood function for a multivariate normal model needs a
  variance-covariance matrix and a mean vector.	 It would be most convenient to
  represent it as a list containing a vector and a matrix.  A typical parameter
  might look like
  \preformatted{
      list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0))).
  }
  However, \code{\link{optim}} cannot operate on functions that take lists as input; it
  only likes numeric vectors.  The solution is conversion:
  \preformatted{
 	ipar <- list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0)))
 	initial.param <- as.relistable(ipar)

 	ll <- function(param.vector)
 	{
	   param <- relist(param.vector)
	   -sum(dnorm(x, mean = param$mean, vcov = param$vcov,
		      log = TRUE))
	   ## NB: dnorm() has no vcov... you should get the point
 	}

 	optim(unlist(initial.param), ll)
  }
  \code{relist} takes two parameters: skeleton and flesh.  Skeleton is a sample
  object that has the right \code{shape} but the wrong content.  \code{flesh}
  is a vector with the right content but the wrong shape.  Invoking
  \preformatted{
    relist(flesh, skeleton)
  }
  will put the content of flesh on the skeleton.  You don't need to specify
  skeleton explicitly if the skeleton is stored as an attribute inside flesh.
  In particular, flesh was created from some object obj with
  \code{unlist(as.relistable(obj))}
  then the skeleton attribute is automatically set.

  As long as \code{skeleton} has the right shape, it should be a precise inverse
  of \code{\link{unlist}}.  These equalities hold:
  \preformatted{
   relist(unlist(x), skeleton) == x
   unlist(relist(y, skeleton)) == y

   x <- as.relistable(x)
   relist(unlist(x)) == x
  }
}% details
\value{
.....................
}
\author{R Core, based on a code proposal by Andrew Clausen.}
\seealso{\code{\link{unlist}}%, ..... MORE ?
}
\examples{
 ipar <- list(mean=c(0, 1), vcov=cbind(c(1, 1), c(1, 0)))
 initial.param <- as.relistable(ipar)
 ul <- unlist(initial.param)
 relist(ul)
 stopifnot(identical(relist(ul), initial.param))
}
\keyword{list}
\keyword{manip}
