\name{relevel}
\alias{relevel}
\title{Reorder Levels of Factor}
\usage{
relevel(x, ref, ...)
}
\arguments{
    \item{x}{An unordered factor.}
    \item{ref}{The reference level.}
    \item{\dots}{Additional arguments for future methods.}
}
\description{
    The levels of a factor a re-ordered so that the level specified by
    \code{ref} is first and the others are moved down. This is useful
    for \code{contr.treatment} contrasts which take the first level as
    the reference.
}
\value{
    A factor of the same length as \code{x}.
}
\references{ ~put references to the literature/web site here ~ }

\author{B.D. Ripley}

\seealso{\code{\link{factor}}, \code{\link{contr.treatment}}}

\examples{
data(warpbreaks)
warpbreaks$tension <- relevel(warpbreaks$tension, ref="M")
summary(lm(breaks ~ wool + tension, data=warpbreaks))
}
\keyword{utilities}
