\name{is.finite}
\title{Which Elements are Finite?}
\usage{is.finite(x)}
\alias{finite}
\description{
\code{is.finite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if the jth element of \code{x} is
finite.

At present, IEEE arithmetic is not supported and this is
simply the same as \code{!is.na(x)}.
}
\keyword{programming}
\keyword{math}

