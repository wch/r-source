\name{All}
\title{Are All / Any Values True?}
\usage{
all(\dots, na.rm=FALSE)
any(\dots, na.rm=FALSE)
}
\alias{any}
\alias{all}
\value{
Given a sequence of logical arguments, \code{all} returns a
logical value indicating whether or not all of the elements of
\code{x} are \code{TRUE}.
The value returned is \code{TRUE} if all the values in \code{x} are
\code{TRUE}, and \code{FALSE} if any the values in \code{x} are
\code{FALSE}.  If \code{x} consists of a mix of \code{TRUE} and
\code{NA} values, then \code{all} returns \code{NA}.

Given a sequence of logical arguments, \code{any} returns a
logical value indicating whether or not any of the elements of
\code{x} are \code{TRUE}.
The value returned is \code{TRUE} if any the values in \code{x} are
\code{TRUE}, and \code{FALSE} if all the values in \code{x} are
\code{FALSE}.  If \code{x} consists of a mix of \code{FALSE} and
\code{NA} values, then \code{any} returns \code{NA}.

For both functions the argument \code{na.rm} indicates whether
\code{NA} values should be removed before the result is computed.
}
\examples{
range(x <- sort(round(rnorm(10) - 1.2,1)))
if(any(x < 0)) cat("x contains negative values\n")
if(all(x < 0)) cat("all x values are negative\n")
}
\keyword{logic}
