\name{plot.formula}
\alias{plot.formula}
\alias{lines.formula}
\alias{points.formula}
\title{Formula Notation for Scatterplots}
\description{
  Specify a scatterplot or add points or lines via a formula.
}
\usage{
\method{plot}{formula}(formula, data = parent.frame(), \dots, subset,
             ylab = varnames[response], ask = TRUE)
\method{points}{formula}(formula, data = parent.frame(), \dots, subset)
\method{lines}{formula}(formula, data = parent.frame(), \dots, subset)
}
\arguments{
  \item{formula}{a \code{\link{formula}}, such as \code{y ~ x}.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{\dots}{Further graphical parameters may also be passed as
    arguments, see \code{\link{par}}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{ylab}{the y label of the plot(s).}
  \item{ask}{logical, see \code{\link{par}}.}
}
\details{
  Both the terms in the formula and the \code{\dots} arguments are
  evaluated in \code{data} enclosed in \code{parent.frame()} if
  \code{data} is a list or a data frame. The terms of the formula and
  those arguments in \code{\dots} that are of the same length as
  \code{data} are subjected to the subsetting specified in
  \code{subset}. If the formula in \code{plot.formula} contains more
  than one non-response term, a series of plots of y against each term
  is given. A plot against the running index can be specified as
  \code{plot(y~1)}.

  If \code{y} is an object (ie. has a \code{\link{class}} attribute)
  then \code{plot.formula} looks for a plot method for that class first.
}
\value{
  These functions are invoked for their side effect of drawing 
  in the active graphics device.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{plot.factor}}.
}
\examples{
data(airquality)
op <- par(mfrow=c(2,1))
plot(Ozone ~ Wind, data = airquality, pch=as.character(Month))
plot(Ozone ~ Wind, data = airquality, pch=as.character(Month),
     subset = Month != 7)
par(op)
}
\keyword{hplot}
