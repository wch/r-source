\name{trace}
\title{Trace all calls to a function.}
\usage{
trace(fun)
untrace(fun)
}
\alias{trace}
\alias{untrace}
\arguments{
\item{fun}{any interpreted \R function (not quoted).}
}
\description{
These are both in very primitive form.
If \code{trace} is called the function named is marked and each time it
is entered the call is printed on the console.
To stop tracing a function use \code{untrace}.
}
\seealso{
\code{\link{debug}}.
}
\examples{
trace(names)
data.frame(x=1:2, y=3:4)
untrace(names)
}
\keyword{programming}
