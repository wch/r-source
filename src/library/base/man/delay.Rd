\name{delay}
\title{Delay Evaluation}
\usage{
delay(x, env=.GlobalEnv)
}
\alias{delay}
\arguments{
\item{x}{an expression.}
\item{env}{an evaluation environment}
}
\description{
  \code{delay} creates a \emph{promise} to evaluate the given
  expression in the specified environment if its value is requested.
  This provides direct access to \emph{lazy evaluation} mechanism
  used by \R for the evaluation of (interpreted) functions.
}
\details{
  This is an experimental feature and its addition is purely
  for evaluation purposes.
}
\value{
  A \emph{promise} to evaluate the expression.
  The value which is returned by \code{delay} can be assigned
  without forcing its evaluation, but any further accesses
  will cause evaluation.
}
\examples{
x <- delay({
    for(i in 1:7)
        cat("yippee!\n")
    10
})

x^2 #- yippee
x^2 #- simple number
}
\keyword{programming}
\keyword{data}
