% File src/library/stats/man/is.empty.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{is.empty.model}
\alias{is.empty.model}
\title{Test if a Model's Formula is Empty}
\description{
  \R's formula notation allows models with no intercept and no
  predictors. These require special handling internally.
  \code{is.empty.model()} checks whether an object describes an empty
  model.
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

\seealso{\code{\link{lm}}, \code{\link{glm}}}

\examples{
y <- rnorm(20)
is.empty.model(y ~ 0)
is.empty.model(y ~ -1)
is.empty.model(lm(y ~ 0))
}
\keyword{models}
