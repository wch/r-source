\name{sets}
\alias{union}
\alias{intersect}
\alias{setdiff}
\alias{is.element}
\title{Set Operations}
\usage{
union(x, y)
intersect(x, y)
setdiff(x, y)
is.element(x, y)
}
\arguments{
    \item{x, y}{vectors (of the same type) contain a sequence of items with no       duplicated values.}
}
\description{
    Performs set union, intersection difference and membership on two vectors.
}
\details{
    Each of \code{union}, \code{intersect} and \code{setdiff} will remove
    any duplicated values in the arguments.
}
\value{
    A vector of the same type as \code{y}.
}
\author{B. D. Ripley}

\examples{
x <- sample(1:20, 10)
y <- sample(3:23, 7)
union(x, y)
intersect(x, y)
setdiff(x, y)
is.element(y, x)
}
\keyword{misc}
