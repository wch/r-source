\name{cat}
\title{Concatenate and Print}
\usage{
cat(\dots , sep="")
}
\alias{cat}
\value{
\code{cat} converts its arguments to character strings, concatenates
them, separating them by the given \code{sep=} string, and then
prints them.

\code{cat} is useful for producing output in user defined functions.
}
\examples{
# print an informative message
cat("iteration = ",iter <- iter + 1,"\n")
}
