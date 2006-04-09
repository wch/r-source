\name{interaction}
\title{Compute Factor Interactions}
\usage{
interaction(\dots, drop = FALSE, sep = ".")
}
\alias{interaction}
\arguments{
  \item{\dots}{the factors for which interaction is to be computed, or a
    single list giving those factors.}
  \item{drop}{if \code{drop} is \code{TRUE}, empty factor levels
    are dropped from the result.  The default is to retain all
    factor levels.}
  \item{sep}{string to construct the new level labels by joining the
    constituent ones.}
}
\description{
  \code{interaction} computes a factor which represents the interaction
  of the given factors.  The result of \code{interaction} is always unordered.
}
\value{
  A factor which represents the interaction of the given factors.
  The levels are labelled as the levels of the individual factors joined
  by \code{sep}, i.e. \code{.} by default.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{factor}}; \code{\link{:}} where \code{f:g} is the same as
  \code{interaction(f,g, sep=":")} when \code{f} and  \code{g} are factors.
}
\examples{
a <- gl(2, 4, 8)
b <- gl(2, 2, 8, label = c("ctrl", "treat"))
s <- gl(2, 1, 8, label = c("M", "F"))
interaction(a, b)
interaction(a, b, s, sep = ":")
}
\keyword{category}
