\name{wilcox.test}
\alias{wilcox.test}
\title{Wilcoxon Rank Sum and Signed Rank Tests}
\description{
  Performs one and two sample Wilcoxon tests on vectors of data.
}
\usage{
wilcox.test(x, y = NULL, alternative = c("two.sided", "less", "greater"),
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE, 
            conf.int = FALSE, conf.level = 0.95)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{y}{an optional numeric vector of data values.}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{mu}{a number specifying an optional location parameter.}
  \item{paired}{a logical indicating whether you want a paired test.}
  \item{exact}{a logical indicating whether an exact p-value should be
    computed.}
  \item{correct}{a logical indicating whether to apply continuity
    correction in the normal approximation for the p-value.}
  \item{conf.int}{a logical indicating whether a confidence interval
    should be computed.}
  \item{conf.level}{confidence level of the interval.}
}
\details{
  If only \code{x} is given, or if both \code{x} and \code{y} are given
  and \code{paired} is \code{TRUE}, a Wilcoxon signed rank test of the
  null that the median of \code{x} (in the one sample case) or of
  \code{x-y} (in the paired two sample case) equals \code{mu} is
  performed.

  Otherwise, if both \code{x} and \code{y} are given and \code{paired}
  is \code{FALSE}, a Wilcoxon rank sum test (equivalent to the
  Mann-Whitney test) is carried out.  In this case, the null hypothesis
  is that the location of the distributions of \code{x} and \code{y}
  differ by \code{mu}.

  By default (if \code{exact} is not specified), an exact p-value is
  computed if the samples contain less than 50 finite values and there
  are no ties.  Otherwise, a normal approximation is used.

  Optionally (if argument \code{conf.int} is true), a nonparametric
  confidence interval for the median (one-sample case) or for the
  difference of the location parameters \code{x-y} is computed.  If
  exact p-values are available, an exact confidence interval is obtained
  by the algorithm described in Bauer (1972).  Otherwise, an asymptotic
  confidence interval is returned.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic with a name
    describing it.}
  \item{parameter}{the parameter(s) for the exact distribution of the
    test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{null.value}{the location parameter \code{mu}.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{the type of test applied.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.int}{a confidence interval for the location parameter.
    (Only present if argument \code{conf.int = TRUE}.)}
}
\references{
  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric statistical inference}.
  New York: John Wiley & Sons.
  Pages 27--33 (one-sample), 68--75 (two-sample).

  David F. Bauer (1972),
  Constructing confidence sets using rank statistics.
  \emph{Journal of the American Statistical Association}
  \bold{67}, 687--690.
}
\seealso{
  \code{\link{kruskal.test}} for testing homogeneity in location
  parameters in the case of two or more samples;
  \code{\link{t.test}} for a parametric alternative under normality
  assumptions.
}
\examples{
## One-sample test.
## Hollander & Wolfe (1973), 29f.
## Hamilton depression scale factor measurements in 9 patients with
##  mixed anxiety and depression, taken at the first (x) and second
##  (y) visit after initiation of a therapy (administration of a
##  tranquilizer).
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")
wilcox.test(y - x, alternative = "less")    # The same.
wilcox.test(y - x, alternative = "less",
            exact = FALSE, correct = FALSE) # H&W large sample
                                            # approximation

## Two-sample test.
## Hollander & Wolfe (1973), 69f.
## Permeability constants of the human chorioamnion (a placental
##  membrane) at term (x) and between 12 to 26 weeks gestational
##  age (y).  The alternative of interest is greater permeability
##  of the human chorioamnion for the term pregnancy.
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox.test(x, y, alternative = "g")        # greater
wilcox.test(x, y, alternative = "greater",
            exact = FALSE, correct = FALSE) # H&W large sample
                                            # approximation

wilcox.test(rnorm(10), rnorm(10, 2), conf.int = TRUE)
}
\keyword{htest}
