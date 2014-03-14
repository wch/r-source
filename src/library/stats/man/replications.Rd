% File src/library/stats/man/replications.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{replications}
\title{Number of Replications of Terms}
\usage{
replications(formula, data = NULL, na.action)
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
  
  Any character vectors in the formula are coerced to factors.
}
\value{
  A vector or list with one entry for each term in the formula giving
  the number(s) of replications for each level. If all levels are
  balanced (have the same number of replications) the result is a
  vector, otherwise it is a list with a component for each terms,
  as a vector, matrix or array as required.

  A test for balance is \code{!is.list(replications(formula,data))}.
}
\references{
  Chambers, J. M., Freeny, A and Heiberger, R. M. (1992)
  \emph{Analysis of variance; designed experiments.}
  Chapter 5 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\author{
  The design was inspired by the S function of the same name described
  in Chambers \emph{et al} (1992).
}

\seealso{\code{\link{model.tables}}}

\examples{
## From Venables and Ripley (2002) p.165.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)
replications(~ . - yield, npk)
}
\keyword{models}
