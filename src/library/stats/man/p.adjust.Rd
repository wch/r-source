% File src/library/stats/man/p.adjust.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{p.adjust}
\alias{p.adjust}
\alias{p.adjust.methods}
\title{Adjust P-values for Multiple Comparisons}
\description{Given a set of p-values, returns p-values adjusted using
  one of several methods.}
\usage{
p.adjust(p, method = p.adjust.methods, n = length(p))

p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
}
\arguments{
  \item{p}{numeric vector of p-values (possibly with
    \code{\link{NA}}s).  Any other \R object is coerced by
    \code{\link{as.numeric}}.}
  \item{method}{correction method, a \code{\link{character}} string.
    Can be abbreviated.}
  \item{n}{number of comparisons, must be at least \code{length(p)};
    only set this (to non-default) when you know what you are doing!}
}
\details{
  The adjustment methods include the \I{Bonferroni} correction
  (\code{"bonferroni"}) in which the p-values are multiplied by the
  number of comparisons.  Less conservative corrections are also
  included by
  \bibcite{Holm (1979)} (\code{"holm"}),
  \bibcite{Hochberg (1988)} (\code{"hochberg"}),
  \bibcite{Hommel (1988)} (\code{"hommel"}),
  \bibcite{Benjamini & Hochberg (1995)} (\code{"BH"} or its alias
  \code{"fdr"}), and
  \bibcite{Benjamini & Yekutieli (2001)} (\code{"BY"}), respectively.
  A pass-through option (\code{"none"}) is also included.
  The set of methods are contained in the \code{p.adjust.methods} vector
  for the benefit of methods that need to have the method as an option
  and pass it on to \code{p.adjust}.

  The first four methods are designed to give strong control of the
  family-wise error rate.  There seems no reason to use the unmodified
  \I{Bonferroni} correction because it is dominated by \I{Holm}'s method, which
  is also valid under arbitrary assumptions.

  \I{Hochberg}'s and \I{Hommel}'s methods are valid when the hypothesis tests
  are independent or when they are non-negatively associated (
  \bibcite{Sarkar, 1998; Sarkar and Chang, 1997}).
  \I{Hommel}'s method is more powerful than
  \I{Hochberg}'s, but the difference is usually small and the \I{Hochberg}
  p-values are faster to compute.

  The \code{"BH"} (aka \code{"fdr"}) and \code{"BY"} methods of
  \I{Benjamini}, \I{Hochberg}, and \I{Yekutieli} control the false discovery rate,
  \I{FDR}, the expected proportion of false discoveries amongst the
  rejected hypotheses.

  The \code{"BY"} correction modifies the \I{BH} procedure by replacing the
  target level \eqn{q} with \eqn{q / \sum_{i=1}^{m} 1/i}, where \eqn{m} is
  the number of tests (Theorem 1.3 in the reference), which controls the
  \I{FDR} under the most general form of dependence structure.  This will
  be more conservative than \code{"BH"}, for small \code{p} even more than
  \I{Bonferroni}, see the example.
  The \I{FDR} as implemented by the \code{"BH"} method is a less stringent
  condition than the family-wise error rate, so it is typically more
  powerful than the others.

  Note that you can set \code{n} larger than \code{length(p)} which
  means the unobserved p-values are assumed to be greater than all the
  observed p for \code{"bonferroni"} and \code{"holm"} methods and equal
  to 1 for the other methods.
}

\value{
  A numeric vector of corrected p-values (of the same length as
  \code{p}, with names copied from \code{p}).
}

\references{
  Benjamini, Y., and Hochberg, Y. (1995).
  Controlling the false discovery rate: a practical and powerful
  approach to multiple testing.
  \emph{Journal of the Royal Statistical Society Series B}, \bold{57},
  289--300.
  \doi{10.1111/j.2517-6161.1995.tb02031.x}.
  %% \url{https://www.jstor.org/stable/2346101}.

  Benjamini, Y., and Yekutieli, D. (2001).
  The control of the false discovery rate in multiple testing under
  dependency.
  \emph{Annals of Statistics}, \bold{29}, 1165--1188.
  \doi{10.1214/aos/1013699998}.

  Holm, S. (1979).
  A simple sequentially rejective multiple test procedure.
  \emph{Scandinavian Journal of Statistics}, \bold{6}, 65--70.
  \url{https://www.jstor.org/stable/4615733}.

  Hommel, G. (1988).
  A stagewise rejective multiple test procedure based on a modified
  Bonferroni test.
  \emph{Biometrika}, \bold{75}, 383--386.
  \doi{10.2307/2336190}.

  Hochberg, Y. (1988).
  A sharper Bonferroni procedure for multiple tests of significance.
  \emph{Biometrika}, \bold{75}, 800--803.
  \doi{10.2307/2336325}.

  Shaffer, J. P. (1995).
  Multiple hypothesis testing.
  \emph{Annual Review of Psychology}, \bold{46}, 561--584.
  \doi{10.1146/annurev.ps.46.020195.003021}.
  (An excellent review of the area.)

  Sarkar, S. (1998).
  Some probability inequalities for ordered MTP2 random variables: a
  proof of Simes conjecture.
  \emph{Annals of Statistics}, \bold{26}, 494--504.
  \doi{10.1214/aos/1028144846}.

  Sarkar, S., and Chang, C. K. (1997).
  The Simes method for multiple hypothesis testing with positively
  dependent test statistics.
  \emph{Journal of the American Statistical Association}, \bold{92},
  1601--1608.
  \doi{10.2307/2965431}.

  Wright, S. P. (1992).
  Adjusted P-values for simultaneous inference.
  \emph{Biometrics}, \bold{48}, 1005--1013.
  \doi{10.2307/2532694}.
  (Explains the adjusted P-value approach.)
}

\seealso{
  \code{pairwise.*} functions such as \code{\link{pairwise.t.test}}.
}

\examples{
require(graphics)

set.seed(123)
x <- rnorm(50, mean = c(rep(0, 25), rep(3, 25)))
p <- 2*pnorm(sort(-abs(x)))

round(p, 3)
round(p.adjust(p), 3)
round(p.adjust(p, "BH"), 3)

## or all of them at once (dropping the "fdr" alias):
p.adjust.M <- p.adjust.methods[p.adjust.methods != "fdr"]
p.adj    <- sapply(p.adjust.M, function(meth) p.adjust(p, meth))
p.adj.60 <- sapply(p.adjust.M, function(meth) p.adjust(p, meth, n = 60))
stopifnot(identical(p.adj[,"none"], p), p.adj <= p.adj.60)

round(p.adj, 3)
## or a bit nicer:
head(round(100 * p.adj[,c(7,1:6)], 2), n=21) # in [percent]:
##       none  holm hochberg hommel bonferroni   BH    BY
##  [1,] 0.00  0.00     0.00   0.00       0.00 0.00  0.01 *)
##  [2,] 0.00  0.10     0.10   0.10       0.11 0.04  0.19 *)
##  [3,] 0.00  0.12     0.12   0.12       0.13 0.04  0.19 *)
##  [4,] 0.01  0.46     0.46   0.42       0.49 0.09  0.43
##  [5,] 0.01  0.48     0.48   0.45       0.53 0.09  0.43
##  .... ..........    ............       ...............
##  .... ..........    ............      ................
## [18,] 0.88 29.06    29.06  27.30      44.03 2.45 11.00
## [19,] 0.94 30.08    30.08  29.14      47.01 2.47 11.13
## [20,] 1.13 35.02    35.02  33.89      56.49 2.82 12.71
## [21,] 2.12 63.45    63.45  57.11     100.00 5.04 22.66
##
## *) The smallest 3 Bonferroni values are smaller than the "BY" ones,
##    (John Maindonald, PR#17136)

## number of rejected H0 ("P" < 0.05):
colSums(p.adj < 0.05)
## holm   hochberg     hommel bonferroni         BH         BY       none 
##   11         11         11         11         20         12         22 

## visual comparison
matplot(p, p.adj, ylab="p.adjust(p, meth)", type = "l", asp = 1, lty = 1:6,
        col = 1:7, main = "P-value adjustments")
legR <- function() {
  legend("bottomright", p.adjust.M, col = 1:7, lty = 1:6, bty = "n", inset = 0.05)
  rug(p) }
legR()

## zoom in & log scale
lim <- c(7e-7, .20)
matplot(p, p.adj, ylab="p.adjust(p, meth)", type = "l", asp = 1, lty = 1:6, col = 1:7,
        main = "P-value adjustments [log-log]", log = "xy", xlim=lim, ylim=lim, las=1)
legR()

## Can work with NAs:
pN <- p; iN <- c(46, 47); pN[iN] <- NA
pN.a <- sapply(p.adjust.M, function(meth) p.adjust(pN, meth))
## The smallest 20 P-values all affected by the NA's :
round((pN.a / p.adj)[1:20, ] , 4)
}
\keyword{htest}
