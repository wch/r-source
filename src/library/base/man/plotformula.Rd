\name{plot.formula}
\title{Formula Notation for Scatterplots}
\usage{
plot.formula(formula, data = NULL, subset, na.action, \dots, ask = TRUE)
plot(y ~ x, \dots)
}
\alias{plot.formula}
\arguments{
  \item{formula}{a \code{\link{formula}}, such as \code{y ~ x}.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default action (\code{na.omit})
    is to omit any incomplete observations.
    The alternative action \code{na.fail} causes \code{plot} to
    print an error message and terminate if there are any incomplete
    observations.}
  \item{\dots}{graphical parameters may also be passed as arguments, see
    \code{\link{par}}. These can include \code{ylab}, which if not
    supplied is set by \code{plot.formula}.}
  \item{ask}{logical, see \code{\link{par}}.}
}
\description{
  Specify a scatterplot via a formula.
}
\value{
  This function is invoked for its side effect of drawing a scatter plot
  in the active graphics window.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{plot.factor}}.
}
\examples{
data(swiss)
op <- par(mfrow=c(2,1))
plot(Fertility ~ Education, data = swiss)
plot(Fertility ~ Education, data = swiss, subset = Education < 20)
par(op)
}
\keyword{hplot}
