\name{line}
\alias{line}
\alias{coef.tukeyline}
%\alias{coefficients.tukeyline}
\alias{residuals.tukeyline}
\alias{fitted.tukeyline}
%\alias{fitted.values.tukeyline}
\alias{print.tukeyline}
\title{Robust Line Fitting}
\description{
  Fit a line robustly as recommended in \emph{Exploratory Data
    Analysis}.
}
\usage{
line(x, y)

%% Note that these are really just aliases to <generic>.lm and hence
%% should have identical arguments, --> ../../base/man/lm.summaries.Rd
\method{coef}{tukeyline}(object, \dots)
\method{residuals}{tukeyline}(object, type, \dots)
\method{fitted}{tukeyline}(object, \dots)
\method{print}{tukeyline}(x, digits = max(3, getOption("digits") - 3), \dots)
}
\arguments{
  \item{x,y}{the arguments can be any way of specifying x-y pairs.}
  \item{object}{a \code{tukeyline} object, typically the result of
    \code{line(*)}.}
  \item{digits}{number of significant digits to use, see \code{\link{print}}.}
  \item{type, \dots}{potentially further arguments, required by generic.}
}
\value{
  An object of class \code{"tukeyline"}.

  Methods are available for the generic functions \code{coef},
  \code{residuals}, \code{fitted}, and \code{print}.
}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis},
  Reading Massachusetts: Addison-Wesley.
}
\seealso{
  \code{\link{lm}}.
}
\examples{
library(eda)
data(cars)
plot(cars)
(z <- line(cars))
abline(coef(z))
## Tukey-Anscombe Plot :
plot(residuals(z) ~ fitted(z), main = deparse(z$call))
}
\keyword{robust}
\keyword{regression}
