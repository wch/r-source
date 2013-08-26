% File src/library/stats/man/printCoefmat.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{printCoefmat}
\alias{printCoefmat}
\title{Print Coefficient Matrices}
\description{
  Utility function to be used in higher-level \code{\link{print}}
  methods, such as those for \code{\link{summary.lm}},
  \code{\link{summary.glm}} and \code{\link{anova}}.  The
  goal is to provide a flexible interface with smart defaults such
  that often, only \code{x} needs to be specified.
}
\usage{
printCoefmat(x, digits = max(3, getOption("digits") - 2),
             signif.stars = getOption("show.signif.stars"),
             signif.legend = signif.stars,
             dig.tst = max(1, min(5, digits - 1)),
             cs.ind = 1L:k, tst.ind = k + 1L,
             zap.ind = integer(), P.values = NULL,
             has.Pvalue = nc >= 4L &&
                          substr(colnames(x)[nc], 1L, 3L) == "Pr(",
             eps.Pvalue = .Machine$double.eps,
             na.print = "NA", \dots)
}
\arguments{
  \item{x}{a numeric matrix like object, to be printed.}
  \item{digits}{minimum number of significant digits to be used for
    most numbers.}
  \item{signif.stars}{logical; if \code{TRUE}, P-values are additionally
    encoded visually as \sQuote{significance stars} in order to help scanning
    of long coefficient tables.  It defaults to the
    \code{show.signif.stars} slot of \code{\link{options}}.}
  \item{signif.legend}{logical; if \code{TRUE}, a legend for the
    \sQuote{significance stars} is printed provided \code{signif.stars =
      TRUE}.}
  \item{dig.tst}{minimum number of significant digits for the test statistics,
    see \code{tst.ind}.}
  \item{cs.ind}{indices (integer) of column numbers which are (like)
    \bold{c}oefficients and \bold{s}tandard errors to be formatted
    together.}
  \item{tst.ind}{indices (integer) of column numbers for test
    statistics.}
  \item{zap.ind}{indices (integer) of column numbers which should be
    formatted by \code{\link{zapsmall}}, i.e., by \sQuote{zapping} values
    close to 0.}
  \item{P.values}{logical or \code{NULL}; if \code{TRUE}, the last
    column of \code{x} is formatted by \code{\link{format.pval}} as P
    values.  If \code{P.values = NULL}, the default, it is set to
    \code{TRUE} only if \code{\link{options}("show.coef.Pvalue")} is
    \code{TRUE} \emph{and} \code{x} has at least 4 columns \emph{and}
    the last column name of \code{x} starts with \code{"Pr("}.}
  \item{has.Pvalue}{logical; if \code{TRUE}, the last column of \code{x}
    contains P values; in that case, it is printed if and only if
    \code{P.values} (above) is true.}
  \item{eps.Pvalue}{number, ..}
  \item{na.print}{a character string to code \code{\link{NA}} values in
    printed output.}
 \item{\dots}{further arguments for \code{\link{print}}.}
}
\value{
  Invisibly returns its argument, \code{x}.
}
\author{Martin Maechler}
\seealso{
  \code{\link{print.summary.lm}},
  \code{\link{format.pval}},
  \code{\link{format}}.
}
\examples{
cmat <- cbind(rnorm(3, 10), sqrt(rchisq(3, 12)))
cmat <- cbind(cmat, cmat[, 1]/cmat[, 2])
cmat <- cbind(cmat, 2*pnorm(-cmat[, 3]))
colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
printCoefmat(cmat[, 1:3])
printCoefmat(cmat)
op <- options(show.coef.Pvalues = FALSE)
printCoefmat(cmat, digits = 2)
printCoefmat(cmat, digits = 2, P.values = TRUE)
options(op) # restore
}
\keyword{print}
