\name{decompose}
\alias{decompose}
\alias{plot.decomposed.ts}
\title{Classical seasonal decomposition by moving averages}
\description{
  Decompose a time series into seasonal, trend and irregular components
  using moving averages. Deals either with additive or multiplicative
  seasonal component.
}
\usage{
decompose(x, type = "additive")}
\arguments{
  \item{x}{A time series.}
  \item{type}{Either \code{additive} or \code{multiplicative}.}
}
\details{
The additive model used is:

Y[t] = T[t] + S[t] + e[t]

The multiplicative model used is:

Y[t] = T[t] * S[t] + e[t]
}
\note{
  The function \code{stl} provides a much more sophisticated way for
  decomposition.
}
\value{
  An object of class \code{decomposed.ts} with following components:
  \item{seasonal}{The seasonal component (i.e., the repeated seasonal figure)}
  \item{figure}{The estimated seasonal figure only}
  \item{trend}{The trend component}
  \item{random}{The reminder part}
  \item{type}{The value of \code{type}}
}
\author{
  David Meyer \email{david.meyer@ci.tuwien.ac.at}
}
\seealso{\code{\link{stl}}}

\examples{
data(co2)
m <- decompose(co2)
m$figure
plot(m)
}
\keyword{ts}
