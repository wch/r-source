\name{terms}
\title{Model Terms}
\usage{
terms(object, \dots)
terms.formula(formula.obj, keep.order=FALSE)
terms.terms(terms.obj)
terms.default(anything)
}
\alias{terms}
\alias{terms.formula}
\alias{terms.terms}
\alias{terms.aovlist}
\alias{terms.default}
\alias{print.terms}
\description{
The function \code{terms} is a generic function
which can be used to extract \emph{terms} objects
from various kinds of \R data objects.
}
\value{
All the functions above produce an object
of class \code{terms} which contains the
\emph{terms} representation of a symbolic model.
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link{formula}}.
}
%%--- MM: I would really like instructive examples here...
\keyword{models}
