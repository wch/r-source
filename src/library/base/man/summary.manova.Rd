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
        intercept = FALSE, \dots)
}
\arguments{
  \item{object}{An object of class \code{"manova"} or an \code{aov}
    object with multiple responses.}
  \item{test}{The name of the test statistic to be used. Partial
    matching is used so the name can be abbreviated.}
  \item{intercept}{logical. If \code{TRUE}, the intercept term is
    included in the table.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The \code{summary.manova} method uses a multivariate test statistic
  for the summary table.  Wilks' statistic is most popular in the
  literature, but the default Pillai-Bartlett statistic is recommended
  by Hand and Taylor (1987).
}
\value{
  A list with components
  \item{SS}{A named list of sums of squares and product matrices.}
  \item{Eigenvalues}{A matrix of eigenvalues, }
  \item{stats}{A matrix of the statistics, approximate F value and
    degrees of freedom.}
}
\references{
  Krzanowski, W. J. (1988) \emph{Principles of Multivariate Analysis. A
    User's Perspective.} Oxford.

  Hand, D. J. and Taylor, C. C.  (1987)
  \emph{Multivariate Analysis of Variance and Repeated Measures.}
  Chapman and Hall.
}
\seealso{
  \code{\link{manova}}, \code{\link{aov}}
}

\examples{
## Example on producing plastic film from Krzanowski (1998, p. 381)
tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(tear, gloss, opacity)
rate <- factor(gl(2,10), labels=c("Low", "High"))
additive <- factor(gl(2, 5, len=20), labels=c("Low", "High"))

fit <- manova(Y ~ rate * additive)
summary.aov(fit)           # univariate ANOVA tables
summary(fit, test="Wilks") # ANOVA table of Wilks' lambda
}
\keyword{models}
