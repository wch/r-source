\name{args}
\title{Argument List of a Function}
\usage{
args(name)
}
\alias{args}
\arguments{
\item{name}{an interpreted function.
If \code{name} is a character string then the function
with that name is found and used.}
}
\value{
A function with identical formal argument list but an empty body.
This function is mainly used interactively.
For programming, use \code{\link{formals}} instead.
}
\seealso{
\code{\link{formals}}, \code{\link{help}}.
}
\examples{
args(c) #-> NULL (c is a 'primitive' function).
args(plot.default)
}
\keyword{documentation}


