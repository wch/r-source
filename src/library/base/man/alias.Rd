\name{alias}
\title{Find Aliases (Dependencies) in a Model }
\usage{
alias(object, ...)
}
\usage{
alias.formula(object, data, ...)
}
\usage{
alias.lm(object, complete = T, partial = F)
}
\alias{alias}
\alias{alias.formula}
\alias{alias.lm}
\arguments{
  \item{object}{A fitted model object, for example from \code{lm} or
    \code{aov}, or a formula for \code{alias.formula}.}
  \item{data}{Optionally,a data frame to search for the objects in the formula.}
 \item{complete}{Should information on complete aliasing be included?}
 \item{partial}{Should information on partial aliasing be included?}
}
\description{
  Although the main method is for class \code{"lm"}, \code{alias} is
  most useful for experimental designs and so is used with fits from
  \code{aov}.
  Complete aliasing refers to effects in linear models that cannot be estimated
  independently of the terms which occur earlier in the model and so
  have their coefficients omitted from the fit. Partial aliasing refers
  to effects that can be estimated less precisely because of
  correlations induced by the design.
}
\value{
  A list containing components
  \item{Model}{Description of the model; usually the formula.}
  \item{Complete}{A matrix with columns corresponding to effects that
    are linearly dependent on the rows.}
  \item{Partial}{The correlations of the estimable effects, with a zero
    diagonal.}
}
\examples{
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

# The next line is optional.
library(MASS) # for fractions package which gives neater results. 
npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)
npk.aov <- aov(yield ~ block + N*P*K, npk)
alias(npk.aov)
}
\author{B.D. Ripley}
\keyword{models}
