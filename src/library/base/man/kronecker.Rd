\name{kronecker}

\title{Kronecker products on arrays}

\usage{
kronecker(X, Y, FUN = "*", make.dimnames = FALSE, ...)
X \%x\% Y
}

\alias{kronecker}
\alias{\%x\%}

\arguments{
 \item{X}{A vector or array.}
 \item{Y}{A vector or array.}
 \item{FUN}{a function; it may be a quoted string.}
 \item{make.dimnames}{Provide dimnames that are the product of the
dimnames of \code{X} and \code{Y}.}
 \item{\dots}{optional arguments to be passed to \code{FUN}.}
}

\description{
Computes the generalised kronecker product of two arrays,
\code{X} and \code{Y}.  \code{kronecker(X, Y)} returns an array
\code{A} with dimensions \code{dim(X) * dim(Y)}.}

\details{If \code{X} and \code{Y} do not have the same number of
dimensions, the smaller array is padded with dimensions of size
one.  The returned array comprises submatrices constructed by
taking \code{X} one term at a time and expanding that term as
\code{FUN(x, Y, ...)}.

\code{\%x\%} is an alias for \code{kronecker} (where
\code{FUN} is hardwired to \code{"*"}).
}

\references{Matrix Algebra Useful for Statistics, Shayle
R. Searle, John Wiley and Sons, 1982.}

\author{Jonathan Rougier, \email{J.C.Rougier@durham.ac.uk}}

\seealso{\code{\link{outer}}, on which \code{kronecker} is built
and \code{\link{\%*\%}} for usual matrix multiplication.}

\examples{
# simple scalar multiplication
( M <- matrix(1:6, ncol=2) )
stopifnot(kronecker(4, M)==4 * M)
# Block diagonal matrix:
stopifnot(kronecker(diag(1, 3), M) == diag(1, 3) \%x\% M)

# ask for dimnames

fred <- matrix(1:12, 3, 4, dimnames=list(LETTERS[1:3], LETTERS[4:7]))
bill <- c("happy" = 100, "sad" = 1000)
kronecker(fred, bill, make.dimnames = TRUE)

bill <- outer(bill, c("cat"=3, "dog"=4))
kronecker(fred, bill, make.dimnames = TRUE)

\testonly{

# dimnames are hard work: let's test them thoroughly

dimnames(bill) <- NULL
kronecker(fred, bill, make=TRUE)
kronecker(bill, fred, make=TRUE)

dim(bill) <- c(2, 2, 1)
dimnames(bill) <- list(c("happy", "sad"), NULL, "")
kronecker(fred, bill, make=TRUE)

bill <- array(1:24, c(3, 4, 2))
dimnames(bill) <- list(NULL, NULL, c("happy", "sad"))
kronecker(bill, fred, make=TRUE)
kronecker(fred, bill, make=TRUE)

fred <- outer(fred, c("frequentist"=4, "bayesian"=4000))
kronecker(fred, bill, make=TRUE)
}
}

\keyword{array}
