\name{glm.summary}
\alias{summary.glm}
\alias{summary.glm.null}
\alias{print.summary.glm}
\alias{print.summary.glm.null}
\title{Summarizing Generalized Linear Model Fits}
\usage{
\method{summary}{glm}(object, dispersion = NULL, correlation = FALSE, \dots)

\method{print}{summary.glm}(x, digits = max(3, getOption("digits") - 3),
              na.print = "", symbolic.cor = p > 4,
              signif.stars = getOption("show.signif.stars"), \dots)
}
\arguments{
  \item{object}{an object of class \code{"glm"}, usually, a result of a
    call to \code{\link{glm}}.}
  \item{x}{an object of class \code{"summary.glm"}, usually, a result of a
    call to \code{summary.glm}.}
  \item{dispersion}{the dispersion parameter for the fitting family.
    By default it is obtained from \code{object}.}
  \item{correlation}{logical; if \code{TRUE}, the correlation matrix of
    the estimated parameters is returned and printed.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{na.print}{Unused.}
  \item{symbolic.cor}{logical. If \code{TRUE}, print the correlations in
    a symbolic form (see \code{\link{symnum}} rather than as numbers.}
  \item{signif.stars}{logical. If \code{TRUE}, ``significance stars''
    are printed for each coefficient.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  These functions are all \code{\link{methods}} for class \code{glm} or
  \code{summary.glm} objects.
}
\details{
\code{print.summary.glm} tries to be smart about formatting the
coefficients, standard errors, etc. and additionally gives
``significance stars'' if \code{signif.stars} is \code{TRUE}.
}
\seealso{
  \code{\link{glm}}, \code{\link{summary}}.
}
\examples{
## --- Continuing the Example from  ``?glm'':%\code{\link{glm}}:
\testonly{example("glm", echo = FALSE)}
summary(glm.D93)
}
\keyword{models}
\keyword{regression}

