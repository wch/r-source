\name{is.empty.model}
\alias{is.empty.model}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Check if a model is empty}
\description{
\R model notation allows models with no intercept and no predictors. These require special handling internally.  \code{is.empty.model()} checks whether an object describes an empty model.
}
\usage{
is.empty.model(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{x}{A \code{terms} object or an object with a \code{terms} method.}
}

\value{
	\code{TRUE} if the model is empty
}

\seealso{ \code{\link{lm}},\code{\link{glm}}}

\examples{
y<-rnorm(20)
is.empty.model(y~0)
is.empty.model(y~-1)
is.empty.model(lm(y~0))
}
\keyword{models}%-- one or more ...
