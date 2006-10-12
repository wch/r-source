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
  \item{\dots}{Arguments to be passed to or from other methods.
    \code{horizontal = TRUE} is also accepted.}
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
  is given.  A plot against the running index can be specified as
  \code{plot(y ~ 1)}.

  Missing values are not considered in these methods, and in particular
  cases with missing values are not removed.

  If \code{y} is an object (i.e. has a \code{\link[base]{class}} attribute)
  then \code{plot.formula} looks for a plot method for that class first.
  Otherwise, the class of \code{x} will determine the type of the plot.
  For factors this will be a parallel boxplot, and argument
  \code{horizontal = TRUE} can be used (see \code{\link{boxplot}}).
}
\value{
  These functions are invoked for their side effect of drawing 
  in the active graphics device.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{points}}, \code{\link{lines}},
  \code{\link{plot.factor}}.
}
\examples{
op <- par(mfrow=c(2,1))
plot(Ozone ~ Wind, data = airquality, pch=as.character(Month))
plot(Ozone ~ Wind, data = airquality, pch=as.character(Month),
     subset = Month != 7)
par(op)
}
\keyword{hplot}
