\name{structure}
\title{Attribute Specification}
\usage{
structure(data, \dots)
}
\alias{structure}
\arguments{
\item{data}{an object which will have
various attributes attached to it.}
\item{\dots}{attributes, specified in \code{tag=value}
form, which will be attached to data.}
}
\description{
\code{structure} returns the given object with its attributes set.
}
\examples{
structure(1:6, dim = 2:3)
}
\keyword{attribute}
\keyword{manip}
