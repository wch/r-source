\name{interaction}
\title{Compute Factor Interactions}
\usage{
interaction(\dots, drop = FALSE)
}
\alias{interaction}
\arguments{
  \item{\dots}{the factors for which interaction is to be computed, or a
    single list giving those factors.}
  \item{drop}{if \code{drop} is \code{TRUE}, empty factor levels
    are dropped from the result.  The default is to retain all
    factor levels.}
}
\description{
  \code{interaction} computes a factor which represents the interaction
  of the given factors.  The result of \code{interaction} is always unordered.
}
\value{
  A factor which represents the interaction of the given factors.
  The levels are labelled as the levels of the individual factors joined
  by \code{.}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{factor}}, \code{\link{:}}
}

\examples{
a <- gl(2, 2, 8)
b <- gl(2, 4, 8)
interaction(a, b)
}
\keyword{category}
