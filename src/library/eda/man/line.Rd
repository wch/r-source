\name{line}
\title{Robust Line Fitting}
\usage{
line(x, y)

coefficients(tukeyline.obj)
residuals(tukeyline.obj)
fitted.values(tukeyline.obj)
print(tukeyline.obj)
}
\alias{coefficients.tukeyline}
\alias{residuals.tukeyline}
\alias{fitted.values.tukeyline}
\alias{print.tukeyline}
\arguments{
  \item{x,y}{the arguments can be any way of specifying x-y pairs.}
}
\description{
  This function fits a line robustly as recommended in John Tukey's book
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
