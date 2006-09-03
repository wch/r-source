\name{identical}
\alias{identical}
\title{Test Objects for Exact Equality}
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

  The function \code{all.equal} is also
  sometimes used to test equality this way, but was intended for
  something different:  It allows for \dQuote{reasonable}
  differences in numeric results.

  The computations in \code{identical} are also reliable and usually
  fast.  There should never be an error.  The only known way to kill
  \code{identical} is by having an invalid pointer at the C level,
  generating a memory fault.  It will usually find inequality quickly.
  Checking equality for two large, complicated objects can take longer
  if the objects are identical or nearly so, but represent completely
  independent copies.  For most applications, however, the computational cost
  should be negligible.

  \code{identical} sees \code{NaN} as different from
  \code{as.double(NA)}, but all \code{NaN}s are equal (and all \code{NA}
  of the same type are equal).

  Comparison of character strings allows for embedded \code{nul}
  characters.  Comparison of attributes view them as a set (and not a
  vector, so order is not tested).
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
  \code{\link{isTRUE}} is a simple wrapper based on \code{identical}.
}
\examples{
identical(1, NULL) ## FALSE -- don't try this with ==
identical(1, 1.)   ## TRUE in R (both are stored as doubles)
identical(1, as.integer(1)) ## FALSE, stored as different types

x <- 1.0; y <- 0.99999999999
## how to test for object equality allowing for numeric fuzz :
(E <- all.equal(x,y))
isTRUE(E) # which is simply defined to just use
identical(TRUE, E)
## If all.equal thinks the objects are different, it returns a
## character string, and the above expression evaluates to FALSE

# even for unusual R objects :
identical(.GlobalEnv, environment())
}
\keyword{ programming }
\keyword{ logic }
\keyword{ iteration }
