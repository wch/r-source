\name{is.finite}
\title{Finite, Infinite and NaN Numbers}
\usage{
is.finite(x)
is.infinite(x)
Inf
NaN
is.nan(x)
}
\alias{is.finite}
\alias{is.infinite}
\alias{Inf}
\alias{NaN}
\alias{is.nan}
\description{
\code{is.finite} and \code{is.infinite} return a vector of the same
length as \code{x}, indicating which elements are finite or not.

\code{Inf} and \code{-Inf} are positive and negative `infinity' whereas
\code{NaN} means ``Not a Number''.
}
\arguments{
  \item{x}{(numerical) object to be tested.}
}
\details{
\code{is.finite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if \code{x[j]} is
finite (i.e. it is not one of the values \code{NA}, \code{NaN},
\code{Inf} or \code{-Inf}).

\code{is.infinite} returns a vector of the same length as \code{x}
the jth element of which is \code{TRUE} if \code{x[j]} is
infinite (i.e. equal to one of \code{Inf} or \code{-Inf}).
}
\note{
    In \R, basically all mathematical functions (including basic
    \code{\link{Arithmetic}}), are supposed to work properly with
    \code{+/- Inf} and \code{NaN} as input or output.

    The basic rule should be that calls and relations with \code{Inf}s
    really are statements with a proper mathematical \emph{limit}, see the many
    examples below.
}
\seealso{\code{\link{NA}}, \emph{`Not Available'} which is not a number
    as well, however usually used for missing values.
}
\references{
%%- better ones ?!??
    ANSI/IEEE 754 Floating-Point Standard.

    Currently (6/2002), Bill Metzenthen's \email{billm@suburbia.net} tutorial
    and examples at \cr \url{http://www.suburbia.net/~billm/}
}
\examples{
pi / 0 ## = Inf a non-zero number divided by zero creates infinity
0 / 0  ## =  NaN

1/0 + 1/0# Inf
1/0 - 1/0# NaN

stopifnot(
    1/0 == Inf,
    1/Inf == 0
)
sin(Inf)
cos(Inf)
tan(Inf)
}
\keyword{programming}
\keyword{math}
