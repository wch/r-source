\name{print.coefmat}
\title{Print Coefficients' Matrices}
\usage{
print.coefmat(x, digits=max(3, .Options$digits - 2),
              signif.stars = .Options$show.signif.stars,
              dig.tst = max(1, min(5, digits - 1)),
              cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
              has.Pvalue = d[2] >= 4 && substr(colnames(x)[d[2]],1,3) == "Pr(",
              \dots)
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
 \item{cs.ind}{indices (integer) of column numbers which are (like)
   \bold{c}oefficients and \bold{s}tandard errors to be formatted together.}
 \item{tst.ind}{indices (integer) of column numbers for test statistics.}
 \item{zap.ind}{indices (integer) of column numbers which should be
   formatted by \code{\link{zapsmall}(.)}, i.e., by ``zapping'' values
   close to 0.}
 \item{has.Pvalue}{logical; if \code{TRUE}, the last column of \code{x}
   is formatted by \code{\link{format.pval}} as P values.}
 \item{\dots}{Further arguments for \code{print(..)}.}
}
\description{
  Utility function to be used in ``higher level'' \code{\link{print}}
  methods,  such as \code{\link{print.summary.lm}} and
  \code{\link{print.summary.glm}}.  The goal is to provide a flexible
  interface with smart defaults such that often, only \code{x} needs to
  be specified.
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
cmat <- cbind(Estimate = rnorm(3, 10), Std.Err = sqrt(rchisq(3, 12)))
cmat <- cbind(cmat, z.val = cmat[,1]/cmat[,2])
cmat <- cbind(cmat, P.val = 2*pnorm(-cmat[,3]))
print.coefmat(cmat)
print.coefmat(cmat, digits=2, has.P = TRUE)
}
\keyword{print}
