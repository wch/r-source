% File src/library/datasets/man/chickwts.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{chickwts}
\docType{data}
\alias{chickwts}
\title{Chicken Weights by Feed Type}
\description{
  An experiment was conducted to measure and compare the effectiveness
  of various feed supplements on the growth rate of chickens.
}
\usage{chickwts}
\format{
  A data frame with 71 observations on the following 2 variables.
  \describe{
    \item{\code{weight}}{a numeric variable giving the chick weight.}
    \item{\code{feed}}{a factor giving the feed type.}
  }
}
\source{
  Anonymous (1948)
  \emph{Biometrika}, \bold{35}, 214.
}
\details{
  Newly hatched chicks were randomly allocated into six groups, and each
  group was given a different feed supplement.  Their weights in grams
  after six weeks are given along with feed types.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(stats); require(graphics)
boxplot(weight ~ feed, data = chickwts, col = "lightgray",
    varwidth = TRUE, notch = TRUE, main = "chickwt data",
    ylab = "Weight at six weeks (gm)")
anova(fm1 <- lm(weight ~ feed, data = chickwts))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
}
\keyword{datasets}
