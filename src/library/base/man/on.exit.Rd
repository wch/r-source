\name{on.exit}
\title{Function Exit Code}
\usage{
on.exit(expr)
}
\alias{on.exit}
\arguments{
\item{expr}{an expression to be executed.}
}
\description{
\code{on.exit} records the expression given as
its argument as needing to be executed when
then current function exits (either naturally or as the
result of an error).
This is useful for resetting graphical parameters or
performing other cleanup actions.
}
\examples{
opar <- par(mai=c(1,1,1,1))
on.exit(par(opar))
}
\keyword{programming}
