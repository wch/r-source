% File src/library/stats/man/summary.manova.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{summary.manova}
\alias{summary.manova}
\alias{print.summary.manova}
\title{Summary Method for Multivariate Analysis of Variance}
\description{
  A \code{summary} method for class \code{"manova"}.
}
\usage{
\method{summary}{manova}(object,
        test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
        intercept = FALSE, tol = 1e-7, \dots)
}
\arguments{
  \item{object}{An object of class \code{"manova"} or an \code{aov}
    object with multiple responses.}
  \item{test}{The name of the test statistic to be used.  Partial
    matching is used so the name can be abbreviated.}
  \item{intercept}{logical.  If \code{TRUE}, the intercept term is
    included in the table.}
  \item{tol}{tolerance to be used in deciding if the residuals are
    rank-deficient: see \code{\link{qr}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The \code{summary.manova} method uses a multivariate test statistic
  for the summary table.  Wilks' statistic is most popular in the
  literature, but the default Pillai--Bartlett statistic is recommended
  by Hand and Taylor (1987).

  The table gives a transformation of the test statistic which has
  approximately an F distribution.  The approximations used follow
  S-PLUS and SAS (the latter apart from some cases of the
  Hotelling--Lawley statistic), but many other distributional
  approximations exist: see Anderson (1984) and Krzanowski and Marriott
  (1994) for further references.  All four approximate F statistics are
  the same when the term being tested has one degree of freedom, but in
  other cases that for the Roy statistic is an upper bound.

  The tolerance \code{tol} is applied to the QR decomposition of the
  residual correlation matrix (unless some response has essentially zero
  residuals, when it is unscaled).  Thus the default value guards
  against very highly correlated responses: it can be reduced but doing
  so will allow rather inaccurate results and it will normally be better
  to transform the responses to remove the high correlation.
}
\value{
  An object of class \code{"summary.manova"}.  If there is a positive
  residual degrees of freedom, this is a list with components
  \item{row.names}{The names of the terms, the row names of the
    \code{stats} table if present.}
  \item{SS}{A named list of sums of squares and product matrices.}
  \item{Eigenvalues}{A matrix of eigenvalues.}
  \item{stats}{A matrix of the statistics, approximate F value,
    degrees of freedom and P value.}
  otherwise components \code{row.names}, \code{SS} and \code{Df}
  (degrees of freedom) for the terms (and not the residuals).
}
\references{
  Anderson, T. W. (1994) \emph{An Introduction to Multivariate
    Statistical Analysis.} Wiley.

  Hand, D. J. and Taylor, C. C.  (1987)
  \emph{Multivariate Analysis of Variance and Repeated Measures.}
  Chapman and Hall.

  Krzanowski, W. J. (1988) \emph{Principles of Multivariate Analysis. A
    User's Perspective.} Oxford.

  Krzanowski, W. J. and Marriott, F. H. C. (1994) \emph{Multivariate
    Analysis. Part I: Distributions, Ordination and Inference.} Edward Arnold.
}
\seealso{
  \code{\link{manova}}, \code{\link{aov}}
}

\examples{\donttest{
## Example on producing plastic film from Krzanowski (1998, p. 381)
tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(tear, gloss, opacity)
rate <- factor(gl(2,10), labels = c("Low", "High"))
additive <- factor(gl(2, 5, length = 20), labels = c("Low", "High"))

fit <- manova(Y ~ rate * additive)
summary.aov(fit)             # univariate ANOVA tables
summary(fit, test = "Wilks") # ANOVA table of Wilks' lambda
summary(fit)                # same F statistics as single-df terms
}}
\keyword{models}
