\name{is.finite}
\title{Which Elements are Finite or Infinite?}
\usage{is.finite(x)}
\usage{is.infinite(x)}
\alias{is.finite}
\alias{is.infinite}
\description{
\code{is.finite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if the jth element of \code{x} is
finite (i.e. it is not one of the values \code{NA}, \code{NaN},
\code{Inf} or \code{-Inf}).

\code{is.infinite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if the jth element of \code{x} is
infinite (i.e. equal to one of \code{Inf} or \code{-Inf}).
}
\keyword{programming}
\keyword{math}

