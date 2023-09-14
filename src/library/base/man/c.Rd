% File src/library/base/man/c.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later
\name{c}
\title{Combine Values into a Vector or List}
\alias{c}
\alias{c.default}% existing "conceptually", see ../../tools/R/QC.R
\usage{
## S3 Generic function
c(\dots)

\S3method{c}{default}(\dots, recursive = FALSE, use.names = TRUE)
}
\description{
  This is a generic function which combines its arguments.

  The default method combines its arguments to form a vector.
  All arguments are coerced to a common type which is the type
  of the returned value, and all attributes except names are removed.
}
\arguments{
  \item{\dots}{objects to be concatenated.  All \code{\link{NULL}} entries
    are dropped before method dispatch unless at the very beginning of the
    argument list.}
  \item{recursive}{logical.  If \code{recursive = TRUE}, the function
    recursively descends through lists (and pairlists) combining all
    their elements into a vector.}
  \item{use.names}{logical indicating if \code{\link{names}} should be
    preserved.}
}
\details{
  The output type is determined from the highest type of the components
  in the hierarchy NULL < raw < logical < integer < double < complex < character
  < list < expression.  Pairlists are treated as lists, whereas non-vector
  components (such as \code{\link{name}}s / \code{symbol}s and \code{\link{call}}s)
  are treated as one-element \code{\link{list}}s
  which cannot be unlisted even if \code{recursive = TRUE}.

    There is a \code{\link{c.factor}} method which combines factors into
  a factor.

  \code{c} is sometimes used for its side effect of removing attributes
  except names, for example to turn an \code{\link{array}} into a vector.
  \code{as.vector} is a more intuitive way to do this, but also drops
  names.  Note that \code{c} methods other than the default are not required
  to remove attributes (and they will almost certainly preserve a class attribute).

  This is a \link{primitive} function.
}
\value{
  \code{NULL} or an expression or a vector of an appropriate mode.
  (With no arguments the value is \code{NULL}.)
}
\section{S4 methods}{
  This function is S4 generic, but with argument list
  \code{(x, ...)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{unlist}} and \code{\link{as.vector}} to produce
  attribute-free vectors.
}
\examples{
c(1, 7:9)
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
ll <- list(A = 1, c = "C")
## do *not* use
c(ll, d = 1:3) # which is == c(ll, as.list(c(d = 1:3)))
## but rather
c(ll, d = list(1:3))  # c() combining two lists

## descend through lists:
c(list(A = c(B = 1)), recursive = TRUE)
c(list(A = c(B = 1, C = 2), B = c(E = 7)), recursive = TRUE)
}
\keyword{manip}
