\name{na.omit.ts}
\alias{na.omit.ts}
\title{na.omit Method for Time Series}
\usage{
na.omit.ts(frame)
}
\arguments{
 \item{frame}{a univariate or multivariate time series.}
}
\description{
    Initial and final segments with missing values in one or more of the
    series are omit. `Internal' missing values will lead to failure.
}
\value{
  A time series without missing values.
}
\author{B. D. Ripley}
\seealso{\code{\link{na.omit}}}

\examples{
data(sales)
sales1 <- ts.union(sales, lead3=lag(sales.lead, -3))
na.omit.ts(sales1)
}

\keyword{ts}
