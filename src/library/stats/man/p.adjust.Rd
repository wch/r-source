\name{p.adjust}
\alias{p.adjust}
\alias{p.adjust.methods}
\title{Adjust p-values for multiple comparisons}
\description{Given a set of p-values, returns p-values adjusted using
  one of several methods.}
\usage{
p.adjust(p, method = p.adjust.methods, n = length(p))

p.adjust.methods
}
\arguments{
  \item{p}{vector of p-values}
  \item{method}{correction method}
  \item{n}{number of comparisons}
}
\value{A vector of corrected p-values.}
\details{
  The adjustment methods include the Bonferroni correction
  (\code{"bonferroni"}) in which the p-values are multiplied by the
  number of comparisons.  Four less conservative corrections are also
  included by Holm (1979) (\code{"holm"}), Hochberg (1988)
  (\code{"hochberg"}), Hommel (1988) (\code{"hommel"}) and Benjamini &
  Hochberg (1995) (\code{"fdr"}), respectively.
  A pass-through option (\code{"none"}) is also included.
  The set of methods are contained in the \code{p.adjust.methods} vector
  for the benefit of methods that need to have the method as an option
  and pass it on to \code{p.adjust}.

  The first four methods are designed to give strong control of the
  family wise error rate.  There seems no reason to use the unmodified
  Bonferroni correction because it is dominated by Holm's method, which
  is also valid under arbitrary assumptions.

  Hochberg's and Hommel's methods are valid when the hypothesis tests
  are independent or when they are non-negatively associated (Sarkar,
  1998; Sarkar and Chang, 1997).  Hommel's method is more powerful than
  Hochberg's, but the difference is usually small and the Hochberg
  p-values are faster to compute.

  The \code{"fdr"} method of Benjamini and Hochberg (1995) controls the
  false discovery rate, the expected proportion of false discoveries
  amongst the rejected hypotheses.  The false discovery rate is a less
  stringent condition than the family wise error rate, so Benjamini and
  Hochberg's method is more powerful than the other methods.
}

\references{
  Benjamini, Y., and Hochberg, Y. (1995).
  Controlling the false discovery rate: a practical and powerful
  approach to multiple testing.
  \emph{Journal of the Royal Statistical Society Series} B, \bold{57},
  289--300.

  Holm, S. (1979).
  A simple sequentially rejective multiple test procedure.
  \emph{Scandinavian Journal of Statistics}, \bold{6}, 65--70.

  Hommel, G. (1988).
  A stagewise rejective multiple test procedure based on a modified
  Bonferroni test.
  \emph{Biometrika}, \bold{75}, 383--386.

  Hochberg, Y. (1988).
  A sharper Bonferroni procedure for multiple tests of significance.
  \emph{Biometrika}, \bold{75}, 800--803.

  Shaffer, J. P. (1995).
  Multiple hypothesis testing.
  \emph{Annual Review of Psychology}, \bold{46}, 561--576.
  (An excellent review of the area.)

  Sarkar, S. (1998).
  Some probability inequalities for ordered MTP2 random variables: a
  proof of Simes conjecture.
  \emph{Annals of Statistics}, \bold{26}, 494--504.

  Sarkar, S., and Chang, C. K. (1997).
  Simes' method for multiple hypothesis testing with positively
  dependent test statistics.
  \emph{Journal of the American Statistical Association}, \bold{92},
  1601--1608.

  Wright, S. P. (1992).
  Adjusted P-values for simultaneous inference.
  \emph{Biometrics}, \bold{48}, 1005--1013.
  (Explains the adjusted P-value approach.)
}

\seealso{
  \code{pairwise.*} functions in the \pkg{stats} package, such
  as \code{\link{pairwise.t.test}}.
}

\examples{
x <- rnorm(50, m=c(rep(0,25),rep(3,25)))
p <- 2*pnorm( -abs(x))

round(p, 3)

round(p.adjust(p), 3)
round(p.adjust(p,"bonferroni"), 3)

round(p.adjust(p,"fdr"), 3)
}
\keyword{htest}
