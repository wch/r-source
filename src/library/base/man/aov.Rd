\name{aov}
\title{Fit an Analysis of Variance Model}
\usage{
aov(formula, data = NULL, projections = FALSE, qr = TRUE,
    contrasts = NULL, \dots)
}
\alias{aov}
\alias{print.aov}
\alias{print.aovlist}
\alias{Error}
\arguments{
  \item{formula}{A formula specifying the model.}
  \item{data}{A data frame in which the variables specified in the
    formula will be found. If missing, the variables are searched for in
    the standard way.}
  \item{projections}{Logical flag: should the projections be returned?}
  \item{qr}{Logical flag: should the QR decomposition be returned?}
  \item{contrasts}{A list of contrasts to be used for some of the factors
    in the formula. These are not used for any \code{Error} term, and
    supplying contrasts for factors only in the \code{Error} term will give
    a warning.}
  \item{\dots}{Arguments to be passed to \code{lm}, such as \code{subset}
    or \code{na.action}.}
}
\description{
  Fit an analysis of variance model by a call to \code{lm} for each stratum.
}
\details{
  This provides a wrapper to \code{lm} for fitting linear models to
  balanced or unbalanced experimental designs.

  The main difference from \code{lm} is in the way \code{print},
  \code{summary} and so on handle the fit: this is expressed in the
  traditional language of the analysis of variance rather than of linear
  models.

  If the formula contains a single \code{Error} term, this is used to
  specify error strata, and appropriate models are fitted within each
  error stratum.

  The formula can specify multiple responses.

  Weights can be specified by a \code{weights} argument, but should not
  be used with an \code{Error} term, and are incompletely supported
  (e.g. not by \code{\link{model.tables}}).
}
\value{
  An object of class \code{c("aov", "lm")} or for multiple responses
  of class \code{c("maov", "aov", "mlm", "lm")} or for multiple error
  strata of class \code{"aovlist"}.  There are
  \code{\link{print}} and \code{\link{summary}} methods available for these.
}
\author{B. D. Ripley}
\seealso{
  \code{\link{lm}}, \code{\link{summary.aov}}, \code{\link{alias}},
  \code{\link{proj}}, \code{\link{model.tables}}, \code{\link{TukeyHSD}}
}
\examples{
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,55.0,
           62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)

( npk.aov <- aov(yield ~ block + N*P*K, npk) )
summary(npk.aov)
coefficients(npk.aov)

## as a test, not particularly sensible statistically
op <- options(contrasts=c("contr.helmert", "contr.treatment"))
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
npk.aovE
summary(npk.aovE)
options(op)# reset to previous
}
\keyword{models}
\keyword{regression}
