\name{line}
\title{Robust Line Fitting}
\usage{
line(x, y)

coef(tukeyline.obj)
residuals(tukeyline.obj)
fitted(tukeyline.obj)
print(tukeyline.obj)
}
\alias{coef.tukeyline}
%\alias{coefficients.tukeyline}
\alias{residuals.tukeyline}
\alias{fitted.tukeyline}
%\alias{fitted.values.tukeyline}
\alias{print.tukeyline}
\arguments{
  \item{x,y}{the arguments can be any way of specifying x-y pairs.}
}
\description{
  This function fits a line robustly as recommended in
  \emph{Exploratory Data Analysis}.
}
\value{
  An object of class \code{tukeyline}.

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
