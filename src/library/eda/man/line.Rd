\name{line}
\alias{line}
\alias{coeff.tukeyline}
\alias{residuals.tukeyline}
\alias{fitted.tukeyline}
\alias{print.tukeyline}
\alias{coef.tukeyline}
\alias{fitted.tukeyline}
\title{Robust Line Fitting}
\usage{
line(x, y)
}
\arguments{
  \item{x,y}{the arguments can be any way of specifying x-y pairs.}
}
\description{
  This function fits a line robustly as recommended in 
  \emph{Exploratory Data Analysis}.
}
\value{
  An object of class \code{tukeyline}.

  Methods are available for the generic functions \code{coefficients},
  \code{residuals}, \code{fitted.values}, and \code{print}.
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
z <- line(cars)
abline(coef(z))
}
\keyword{robust}
\keyword{regression}
