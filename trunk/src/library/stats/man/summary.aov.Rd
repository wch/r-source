% File src/library/stats/man/summary.aov.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

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

  For fits with a single stratum the result will be a list of
  ANOVA tables, one for each response (even if there is only one response):
  the tables are of class \code{"anova"} inheriting from class
  \code{"data.frame"}.  They have columns \code{"Df"}, \code{"Sum Sq"},
  \code{"Mean Sq"}, as well as \code{"F value"} and \code{"Pr(>F)"} if
  there are non-zero residual degrees of freedom.  There is a row for
  each term in the model, plus one for \code{"Residuals"} if there
  are any.

  For multistratum fits the return value is a list of such summaries,
  one for each stratum.
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
## For a simple example see example(aov)

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
        expand.split = FALSE)}
\keyword{models}
\keyword{regression}
