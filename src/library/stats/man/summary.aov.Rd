\name{summary.aov}
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

  For a fits with a single stratum the result will be a list of
  ANOVA tables, one for each response (even if there is only one response):
  the tables are of class \code{"anova"} inheriting from class
  \code{"data.frame"}.

  For multistratum fits the return value is a list of summaries, one for
  each stratum.
}
\note{
  The use of \code{expand.split = TRUE} is little tested: it is always
  possible to set it to \code{FALSE} and specify exactly all
  the splits required.
}
\seealso{
  \code{\link{aov}}, \code{\link{summary}}, \code{\link{model.tables}},
  \code{\link{TukeyHSD}}
}
\examples{
## From Venables and Ripley (2002) p.165.
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

# Cochran and Cox (1957, p.164)
# 3x3 factorial with ordered factors, each is average of 12. 
CC <- data.frame(
    y = c(449, 413, 326, 409, 358, 291, 341, 278, 312)/12,
    P = ordered(gl(3, 3)), N = ordered(gl(3, 1, 9))
)
CC.aov <- aov(y ~ N * P, data = CC , weights = rep(12, 9))
summary(CC.aov)

# Split both main effects into linear and quadratic parts.
summary(CC.aov, split = list(N = list(L = 1, Q = 2),
                             P = list(L = 1, Q = 2)))

# Split only the interaction
summary(CC.aov, split = list("N:P" = list(L.L = 1, Q = 2:4)))

# split on just one var
summary(CC.aov, split = list(P = list(lin = 1, quad = 2)))
summary(CC.aov, split = list(P = list(lin = 1, quad = 2)),
        expand.split=FALSE)}
\keyword{models}
\keyword{regression}
