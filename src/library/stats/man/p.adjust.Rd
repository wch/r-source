% File src/library/stats/man/p.adjust.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
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
  \bibcitet{R:Holm:1979} (\code{"holm"}),
  \bibcitet{R:Hochberg:1988} (\code{"hochberg"}),
  \bibcitet{R:Hommel:1988} (\code{"hommel"}),
  \bibcitet{R:Benjamini+Hochberg:1995} (\code{"BH"} or its alias
  \code{"fdr"}), and
  \bibcitet{R:Benjamini+Yekutieli:2001} (\code{"BY"}), respectively.
  A pass-through option (\code{"none"}) is also included.
  The set of methods are contained in the \code{p.adjust.methods} vector
  for the benefit of methods that need to have the method as an option
  and pass it on to \code{p.adjust}.

  The first four methods are designed to give strong control of the
  family-wise error rate.  There seems no reason to use the unmodified
  \I{Bonferroni} correction because it is dominated by \I{Holm}'s method, which
  is also valid under arbitrary assumptions.

  \I{Hochberg}'s and \I{Hommel}'s methods are valid when the hypothesis tests
  are independent or when they are non-negatively associated
  \bibcitep{R:Sarkar:1998, R:Sarkar+Chang:1997}.
  \I{Hommel}'s method is more powerful than
  \I{Hochberg}'s, but the difference is usually small and the \I{Hochberg}
  p-values are faster to compute.

  The \code{"BH"} (aka \code{"fdr"}) and \code{"BY"} methods of
  \I{Benjamini}, \I{Hochberg}, and \I{Yekutieli} control the false discovery rate,
  the expected proportion of false discoveries amongst the rejected
  hypotheses.  The false discovery rate is a less stringent condition
  than the family-wise error rate, so these methods are more powerful
  than the others.

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
  \bibinfo{R:Shaffer:1995}{footer}{(An excellent review of the area.)}
  \bibinfo{R:Wright:1992}{footer}{(Explains the adjusted P-value approach.)}
  \bibshow{*,
    R:Shaffer:1995,
    R:Wright:1992}
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
noquote(apply(p.adj, 2, format.pval, digits = 3))


## and a graphic:
matplot(p, p.adj, ylab="p.adjust(p, meth)", type = "l", asp = 1, lty = 1:6,
        main = "P-value adjustments")
legend(0.7, 0.6, p.adjust.M, col = 1:6, lty = 1:6)

## Can work with NAs:
pN <- p; iN <- c(46, 47); pN[iN] <- NA
pN.a <- sapply(p.adjust.M, function(meth) p.adjust(pN, meth))
## The smallest 20 P-values all affected by the NAs :
round((pN.a / p.adj)[1:20, ] , 4)
}
\keyword{htest}
