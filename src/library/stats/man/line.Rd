\name{line}
\alias{line}
%\alias{coef.tukeyline}
%\alias{coefficients.tukeyline}
\alias{residuals.tukeyline}
%\alias{fitted.tukeyline}
%\alias{fitted.values.tukeyline}
%\alias{print.tukeyline}
\title{Robust Line Fitting}
\description{
  Fit a line robustly as recommended in \emph{Exploratory Data Analysis}.
}
\usage{
line(x, y)
}
\arguments{
  \item{x,y}{the arguments can be any way of specifying x-y pairs.}
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
require(graphics)

plot(cars)
(z <- line(cars))
abline(coef(z))
## Tukey-Anscombe Plot :
plot(residuals(z) ~ fitted(z), main = deparse(z$call))
}
\keyword{robust}
\keyword{regression}
