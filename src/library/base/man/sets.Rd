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
is.element(x, y)
}
\arguments{
  \item{x, y}{vectors (of the same type) contain a sequence of items
    with no duplicated values.}
}
\description{
  Performs \bold{set} union, intersection, (asymmetric!) difference,
  equality and membership on two vectors.
}
\details{
  Each of \code{union}, \code{intersect} and \code{setdiff} will remove
  any duplicated values in the arguments.

  \code{is.element} is for S compatibility, an \code{\link{.Alias}} for
  \code{\link{\%in\%}} (which is more natural).
}
\value{
  A vector of the same \code{\link{mode}} as \code{x} or \code{y} for
  \code{setdiff} and \code{intersect}, respectively, and
  of a common mode for \code{union}.  A \code{\link{logical}} scalar for
  \code{setequal} and a logical of the same length as \code{x} for
  \code{is.element}.
}
\author{B. D. Ripley}

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
