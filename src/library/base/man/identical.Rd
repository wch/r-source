\name{identical}
\alias{identical}
\title{ Test Objects for Exact Equality }
\description{
  The safe and reliable way to test two objects for being
  \emph{exactly} equal.  It returns \code{TRUE} in this case,
  \code{FALSE} in every other case.
}
\usage{
identical(x, y)
}
\arguments{
  \item{x, y}{any \R objects.}
}
\details{
  A call to \code{identical} is the way to test exact equality in
  \code{if} and \code{while} statements, as well as in logical
  expressions that use \code{&&} or \code{||}.  In all these
  applications you need to be assured of getting a single logical
  value.

  Users often use the comparison operators, such as \code{==} or
  \code{!=}, in these situations.  It looks natural, but it is not what
  these operators are designed to do in R.  They return an object like
  the arguments.  If you expected \code{x} and \code{y} to be of length
  1, but it happened that one of them wasn't, you will \emph{not} get a
  single \code{FALSE}.  Similarly, if one of the arguments is \code{NA},
  the result is also \code{NA}.  In either case, the expression
  \code{if(x == y)....} won't work as expected.

  The function \code{all.equal} is also sometimes used to test equality
  this way, but it was intended for something different.  First, it
  tries to allow for \dQuote{reasonable} differences in numeric results.
  Second, it returns a descriptive character vector instead of
  \code{FALSE} when the objects do not match.  Therefore, it is not the
  right function to use for reliable testing either.  (If you \emph{do} want
  to allow for numeric fuzziness in comparing objects, you can combine
  \code{all.equal} and \code{identical}, as shown in the examples
  below.)

  The computations in \code{identical} are also reliable and usually
  fast.  There should never be an error.  The only known way to kill
  \code{identical} is by having an invalid pointer at the C level,
  generating a memory fault.  It will usually find inequality quickly.
  Checking equality for two large, complicated objects can take longer
  if the objects are identical or nearly so, but represent completely
  independent copies.  For most applications, however, the computational cost
  should be negligible.

  As from \R 1.6.0, \code{identical} sees \code{NaN} as different from
  \code{as.double(NA)}, but all \code{NaN}s are equal (and all \code{NA}
  of the same type are equal).
}
\value{
  A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA}
  and never anything other than a single value.
}
\author{John Chambers}

\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{all.equal}} for descriptions of how two objects differ;
  \link{Comparison} for operators that generate elementwise comparisons.
}
\examples{
identical(1, NULL) ## FALSE -- don't try this with ==
identical(1, 1.)   ## TRUE in R (both are stored as doubles)
identical(1, as.integer(1)) ## FALSE, stored as different types

x <- 1.0; y <- 0.99999999999
## how to test for object equality allowing for numeric fuzz
identical(all.equal(x, y), TRUE)
## If all.equal thinks the objects are different, it returns a
## character string, and this expression evaluates to FALSE

# even for unusual R objects :
identical(.GlobalEnv, environment())
}
\keyword{ programming }
\keyword{ logic }
\keyword{ iteration }
