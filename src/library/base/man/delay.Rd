\name{delay}
\title{Delay Evaluation}
\usage{
delay(expr, env=.GlobalEnv)
}
\alias{delay}
\arguments{
\item{expr}{an expression.}
\item{env}{an evaluation environment}
}
\description{
\code{delay} creates a \emph{promise} to evaluate the given
expression in the specifies environment if its value is requested.
This provides direct access to \emph{lazy evaluation} mechanism
used by \R for the evaluation of (interpreted) functions.

This is an experimental feature and its addition is purely
for evaluation purposes.
}
\value{
A \emph{promise} to evaluate the expression.
The value which is retuned by \code{delay} can be assigned
without forcing its evaluation, but any further accesses
will cause evaluation.
}
\examples{
x <- delay({
    for(i in 1:10)
        print("yippee!\n")
    10
})
}
\keyword{programming}
\keyword{data}
