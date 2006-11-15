\name{c}
\title{Combine Values into a Vector or List}
\usage{
c(\dots, recursive=FALSE)
}
\alias{c}
\description{
  This is a generic function which combines its arguments.

  The default method combines its arguments to form a vector.
  All arguments are coerced to a common type which is the type
  of the returned value, and all attributes except names are removed.
}
\arguments{
  \item{\dots}{objects to be concatenated.}
  \item{recursive}{logical.  If \code{recursive = TRUE}, the function
    recursively descends through lists (and pairlists) combining all
    their elements into a vector.}
}
\details{
  The output type is determined from the highest type of the components
  in the hierarchy NULL < raw < logical < integer < real < complex < character
  < list < expression.  Pairlists are treated as lists, but non-vector
  components (such names and calls) are treated as one-element lists
  which cannot be unlisted even if \code{recursive = TRUE}.

  \code{c} is sometimes used for its side effect of removing attributes
  except names, for example to turn an array into a vector.
  \code{as.vector} is a more intuitive way to do this, but also drops names.
}
\value{
  \code{NULL} or an expression or a vector of an appropriate mode.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{unlist}} and \code{\link{as.vector}} to produce
  attribute-free vectors.
}
\examples{
c(1,7:9)
c(1:5, 10.5, "next")

## uses with a single argument to drop attributes
x <- 1:4
names(x) <- letters[1:4]
x
c(x)          # has names
as.vector(x)  # no names
dim(x) <- c(2,2)
x
c(x)
as.vector(x)

## append to a list:
ll <- list(A = 1, c="C")
## do *not* use
c(ll, d = 1:3) # which is == c(ll, as.list(c(d=1:3))
## but rather
c(ll, d = list(1:3))# c() combining two lists

c(list(A=c(B=1)), recursive=TRUE)

c(options(), recursive=TRUE)
c(list(A=c(B=1,C=2), B=c(E=7)), recursive=TRUE)
}
\keyword{manip}
