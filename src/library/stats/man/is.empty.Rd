\name{is.empty.model}
\alias{is.empty.model}
\title{Check if a Model is Empty}
\description{
\R model notation allows models with no intercept and no predictors. These require special handling internally.  \code{is.empty.model()} checks whether an object describes an empty model.
}
\usage{
is.empty.model(x)
}
\arguments{
 \item{x}{A \code{terms} object or an object with a \code{terms} method.}
}

\value{
  \code{TRUE} if the model is empty
}

\seealso{ \code{\link{lm}},\code{\link{glm}}}

\examples{
y <- rnorm(20)
is.empty.model(y ~ 0)
is.empty.model(y ~ -1)
is.empty.model(lm(y ~ 0))
}
\keyword{models}
