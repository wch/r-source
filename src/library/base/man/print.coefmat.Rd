\name{print.coefmat}
\title{Print Coefficients' Matrices}
\usage{
print.coefmat(x, digits=max(3, .Options$digits - 2),
              signif.stars = .Options$show.signif.stars,
              dig.tst = max(1, min(5, digits - 1)),
              is.cs = NULL, is.tst = NULL, do.zap = FALSE,
              has.Pvalue = substr(colnames(x),1,3) == "Pr(",
              P.values = NULL,
	      na.print = "", \dots)
}
\alias{print.coefmat}
\arguments{
 \item{x}{a numeric matrix like object, to be printed.}
 \item{digits}{number of digits to be used for most numbers.}
 \item{signif.stars}{logical; if \code{TRUE}, P-values are additionally
   encoded visually as ``significance stars'' in order to help scanning of
   long coefficient tables.  It defaults to the \code{show.signif.stars}
   slot of \code{\link{options}(.)}.}
 \item{dig.tst}{number of significant digits for the test statistics,
   see \code{tst.ind}.}
 \item{is.cs}{logical (length nc := \code{ncol(x)}) indicating which
     columns are (like)
   \bold{c}oefficients and \bold{s}tandard errors to be formatted together.}
 \item{is.tst}{logical (length nc) indicating columns for test statistics.}
 \item{do.zap}{logical (length nc) indicating columns which should be
   formatted by \code{\link{zapsmall}(.)}, i.e., by ``zapping'' values
   close to 0.}
 \item{has.Pvalue}{logical (length nc) indicating columns of \code{x}
   containing P values; these are then only printed \emph{iff}
   \code{P.values} (below).}
 \item{P.values}{logical or \code{NULL};
     if \code{TRUE}, the columns of \code{x} where \code{has.Pvalue} is
     \code{TRUE} are formatted by \code{\link{format.pval}} as P values.

     If \code{P.values = NULL}, the default, it is set to \code{TRUE}
     only if \code{link{options}("show.coef.Pvalue")} is \code{TRUE}
     \emph{and} at least one \code{has.Pvalue} is \code{TRUE}.}
 \item{na.print}{a character string to code \code{\link{NA}} values in
    printed output.}
 \item{\dots}{Further arguments for \code{print(..)}.}
}
\description{
  Utility function to be used in ``higher level'' \code{\link{print}}
  methods,  such as \code{\link{print.summary.lm}},
  \code{\link{print.summary.glm}} and \code{\link{print.anova}}.  The
  goal is to provide a flexible interface with smart defaults such that
  often, only \code{x} needs to be specified.
}
\details{
 Despite its name, this is \bold{not} (yet) a method for the generic
 \code{\link{print}} function,  because there is no \code{\link{class}}
 \code{"coefmat"}.
}
\value{Invisibly returns its argument, \code{x}.}}
\author{Martin Maechler}
\seealso{\code{\link{print.pval}}, \code{\link{print.summary.lm}},
  \code{\link{format}}.}
\examples{
cmat <- cbind(rnorm(3, 10), sqrt(rchisq(3, 12)))
cmat <- cbind(cmat, cmat[,1]/cmat[,2])
cmat <- cbind(cmat, 2*pnorm(-cmat[,3]))
colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
print.coefmat(cmat[,1:3])
print.coefmat(cmat)
options(show.coef.Pvalues = FALSE)
print.coefmat(cmat, digits=2)
print.coefmat(cmat, digits=2, P.values = TRUE)
options(show.coef.Pvalues = TRUE)# revert
}
\keyword{print}
