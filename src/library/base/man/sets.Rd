\name{sets}
\alias{union}
\alias{intersect}
\alias{setdiff}
\alias{is.element}
\alias{setequal}
\title{Set Operations}
\usage{
union(x, y)
intersect(x, y)
setdiff(x, y)
setequal(x, y)

is.element(el, set)
}
\arguments{
  \item{x, y, el, set}{vectors (of the same mode) containing a sequence
    of items (conceptually) with no duplicated values.}
}
\description{
  Performs \bold{set} union, intersection, (asymmetric!) difference,
  equality and membership on two vectors.
}
\details{
  Each of \code{union}, \code{intersect}, \code{setdiff} and
  \code{setequal} will discard any duplicated values in the arguments,
  and they apply \code{\link{as.vector}} to their arguments (and so
  in particular coerce factors to character vectors).

  \code{is.element(x, y)} is identical to \code{x \%in\% y}.
}
\value{
  A vector of the same \code{\link{mode}} as \code{x} or \code{y} for
  \code{setdiff} and \code{intersect}, respectively, and
  of a common mode for \code{union}.

  A logical scalar for \code{setequal} and a logical of the same
  length as \code{x} for \code{is.element}.
}
\seealso{\code{\link{\%in\%}}}

\examples{
(x <- c(sort(sample(1:20, 9)),NA))
(y <- c(sort(sample(3:23, 7)),NA))
union(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
setequal(x, y)

## True for all possible x & y :
setequal( union(x,y),
          c(setdiff(x,y), intersect(x,y), setdiff(y,x)))

is.element(x, y)# length 10
is.element(y, x)# length  8
}
\keyword{misc}
