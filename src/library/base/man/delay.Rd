\name{delay-deprecated}
\alias{delay-deprecated}
\title{Delay Evaluation}
\description{
  \code{delay} creates a \emph{promise} to evaluate the given
  expression in the specified environment if its value is requested.
  This provides direct access to \emph{lazy evaluation} mechanism
  used by \R for the evaluation of (interpreted) functions.
}
\usage{
delay(x, env = .GlobalEnv)
}
\arguments{
  \item{x}{an expression.}
  \item{env}{an evaluation environment}
}
\value{
  A \emph{promise} to evaluate the expression.  The value which is
  returned by \code{delay} can be assigned without forcing its
  evaluation, but any further accesses will cause evaluation.
}
\details{
  If promises are kept inside an \code{\link{environment}} or
  \code{\link{list}}, they can be accessed in several ways without
  evaluation, see the examples below.

  When a promise is eventually forced, it is evaluated within the
  environment specified by \code{env} (who contents may have changed in
  the meantime).
}
\note{
  \code{delay} was deprecated in \R 2.1.0 and will be removed in 2.2.0.
}
\examples{
x <- delay({
    for(i in 1:7)
        cat("yippee!\n")
    10
})

x^2 #- yippee
x^2 #- simple number

e <- (function(x, y = 1, z) environment())(1+2, "y", {cat(" HO! "); pi+2})
(le <- as.list(e)) # a list with three promise components
utils::str(le)     # even shows what's promised

le$z # prints; does not eval
ez <- le$z
ez-2 # "HO!", pi
ez # 5.14
le$z # still promise
}
\keyword{programming}
\keyword{data}
