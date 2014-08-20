% File src/library/graphics/man/plotformula.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995--2011 R Core Team
% Distributed under GPL 2 or later

\name{plot.formula}
\alias{plot.formula}
\alias{lines.formula}
\alias{points.formula}
\alias{text.formula}
\title{Formula Notation for Scatterplots}
\description{
  Specify a scatterplot or add points, lines, or text via a formula.
}
\usage{
\method{plot}{formula}(formula, data = parent.frame(), \dots, subset,
             ylab = varnames[response], ask = dev.interactive())

\method{points}{formula}(formula, data = parent.frame(), \dots, subset)

\method{lines}{formula}(formula, data = parent.frame(), \dots, subset)

\method{text}{formula}(formula, data = parent.frame(), \dots, subset)
}
\arguments{
  \item{formula}{a \code{\link{formula}}, such as \code{y ~ x}.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.  A matrix is converted to a data frame.}
  \item{\dots}{Arguments to be passed to or from other methods.
    \code{horizontal = TRUE} is also accepted.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{ylab}{the y label of the plot(s).}
  \item{ask}{logical, see \code{\link{par}}.}
}
\details{
  For the \code{lines}, \code{points} and \code{text} methods the formula
  should be of the form \code{y ~ x} or \code{y ~ 1} with a left-hand
  side and a single term on the right-hand side.  The \code{plot} method
  accepts other forms discussed later in this section.

  Both the terms in the formula and the \code{\dots} arguments are
  evaluated in \code{data} enclosed in \code{parent.frame()} if
  \code{data} is a list or a data frame.  The terms of the formula and
  those arguments in \code{\dots} that are of the same length as
  \code{data} are subjected to the subsetting specified in
  \code{subset}.  A plot against the running index can be specified as
  \code{plot(y ~ 1)}.

  If the formula in the \code{plot} method contains more than one term
  on the right-hand side, a series of plots is produced of the response
  against each non-response term.

  For the \code{plot} method the formula can be of the form
  \code{~ z + y + z}: the variables specified on the right-hand side are
  collected into a data frame, subsetted if specified, and displayed by
  \code{\link{plot.data.frame}}.

  Missing values are not considered in these methods, and in particular
  cases with missing values are not removed.

  If \code{y} is an object (i.e., has a \code{\link{class}} attribute)
  then \code{plot.formula} looks for a plot method for that class first.
  Otherwise, the class of \code{x} will determine the type of the plot.
  For factors this will be a parallel boxplot, and argument
  \code{horizontal = TRUE} can be specified (see \code{\link{boxplot}}).

  Note that some arguments will need to be protected from premature
  evaluation by enclosing them in \code{\link{quote}}: currently this is
  done automatically for \code{main}, \code{sub} and \code{xlab}.  For
  example, it is needed for the \code{panel.first} and \code{panel.last}
  arguments passed to \code{\link{plot.default}}.
}
\value{
  These functions are invoked for their side effect of drawing
  on the active graphics device.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{points}}, \code{\link{lines}},
  \code{\link{plot.factor}}.
}
\examples{
op <- par(mfrow = c(2,1))
plot(Ozone ~ Wind, data = airquality, pch = as.character(Month))
plot(Ozone ~ Wind, data = airquality, pch = as.character(Month),
     subset = Month != 7)
par(op)

## text.formula() can be very natural:
wb <- within(warpbreaks, {
    time <- seq_along(breaks); W.T <- wool:tension })
plot(breaks ~ time, data = wb, type = "b")
text(breaks ~ time, data = wb, label = W.T, col = 1+as.integer(wool))
}
\keyword{hplot}% plot
\keyword{aplot}% points, etc
