\name{aov}
\alias{summary.aov}
\alias{summary.aovlist}
\alias{print.summary.aov}
\alias{print.summary.aovlist}
\title{Summarize an Analysis of Variance Model}
\usage{
\method{summary}{aov}(object, intercept = FALSE, split,
         expand.split = TRUE, keep.zero.df = TRUE, \dots)
\method{summary}{aovlist}(object, \dots)
}
\arguments{
  \item{object}{An object of class \code{"aov"} or \code{"aovlist"}.}
  \item{intercept}{logical: should intercept terms be included?}
  \item{split}{an optional named list, with names corresponding to terms
    in the model.  Each component is itself a list with integer
    components giving contrasts whose contributions are to be summed.}
  \item{expand.split}{logical: should the split apply also to
    interactions involving the factor?}
  \item{keep.zero.df}{logical: should terms with no degrees of freedom
    be included?}
  \item{\dots}{Arguments to be passed to or from other methods,
    for \code{summary.aovlist} including those for \code{summary.aov}.}
}
\description{
  Summarize an analysis of variance model.
}
\value{
  An object of class \code{c("summary.aov", "listof")} or
  \code{"summary.aovlist"} respectively.
}
\author{B. D. Ripley}
\seealso{
  \code{\link{aov}}, \code{\link{summary}}
}
\examples{
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,55.0,
           62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)

( npk.aov <- aov(yield ~ block + N*P*K, npk) )
summary(npk.aov)
coefficients(npk.aov)

## as a test, not particularly sensible statistically
op <- options(contrasts=c("contr.helmert", "contr.treatment"))
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
npk.aovE
summary(npk.aovE)
options(op)# reset to previous
}
\keyword{models}
\keyword{regression}
