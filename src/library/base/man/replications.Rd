% file replications.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{replications}
\title{Number of Replications of Terms}
\usage{
replications(formula, data=NULL, na.action)
}
\alias{replications}
\arguments{
  \item{formula}{a formula or a terms object or a data frame.}
  \item{data}{a data frame used  to  find  the  objects in \code{formula}.}
  \item{na.action}{function for handling missing values.  Defaults to
    a \code{na.action} attribute of \code{data}, then
    a setting of the option \code{na.action}, or \code{na.fail} if that
    is not set.}
}
\description{
  Returns a vector or a list of the number of replicates for
  each term in the formula.
}
\details{
  If \code{formula} is a data frame and \code{data} is missing,
  \code{formula} is used for \code{data} with the formula \code{~ .}.
}
\value{
  A vector or list with one entry for each term in the formula giving
  the number(s) of replications for each level. If all levels are
  balanced (have the same number of replications) the result is a
  vector, otherwise it is a list with a component for each terms,
  as a vector, matrix or array as required.

  A test for balance is \code{!is.list(replications(formula,data))}.
}
\author{B. D. Ripley}

\seealso{\code{\link{model.tables}}}

\examples{
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)
replications(~ . - yield, npk)
}
\keyword{models}
