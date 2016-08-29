% File src/library/base/man/summary.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{summary}
\title{Object Summaries}
\alias{summary}
\alias{summary.default}
\alias{summary.data.frame}
\alias{summary.factor}
\alias{summary.matrix}
\alias{format.summaryDefault}
\alias{print.summaryDefault}
\description{
  \code{summary} is a generic function used to produce result summaries
  of the results of various model fitting functions.  The function
  invokes particular \code{\link{methods}} which depend on the
  \code{\link{class}} of the first argument.
}
\usage{
summary(object, \dots)

\method{summary}{default}(object, \dots, digits)
\method{summary}{data.frame}(object, maxsum = 7,
       digits = max(3, getOption("digits")-3), \dots)

\method{summary}{factor}(object, maxsum = 100, \dots)

\method{summary}{matrix}(object, \dots)

\method{format}{summaryDefault}(x, digits = max(3L, getOption("digits") - 3L), \dots)
 \method{print}{summaryDefault}(x, digits = max(3L, getOption("digits") - 3L), \dots)
}
\arguments{
  \item{object}{an object for which a summary is desired.}
  \item{x}{a result of the \emph{default} method of \code{summary()}.}
  \item{maxsum}{integer, indicating how many levels should be shown for
    \code{\link{factor}}s.}
  \item{digits}{integer, used for number formatting with
    \code{\link{signif}()} (for \code{summary.default}) or
    \code{\link{format}()} (for \code{summary.data.frame}).  In
    \code{summary.default}, if not specified (i.e.,
    \code{\link{missing}(.)}), \code{signif()} will \emph{not} be called
    anymore (since \R >= 3.4.0, where the default has been changed to
    only round in the \code{print} and \code{format} methods).}% for "summaryDefault"
  \item{\dots}{additional arguments affecting the summary produced.}
}
\details{
    For \code{\link{factor}}s, the frequency of the first \code{maxsum - 1}
    most frequent levels is shown, and the less frequent levels are
    summarized in \code{"(Others)"} (resulting in at most \code{maxsum}
    frequencies).

    The functions \code{summary.lm} and \code{summary.glm} are examples
    of particular methods which summarize the results produced by
    \code{\link{lm}} and \code{\link{glm}}.
}
\value{
    The form of the value returned by \code{summary} depends on the
    class of its argument.  See the documentation of the particular
    methods for details of what is produced by that method.

    The default method returns an object of class
    \code{c("summaryDefault", "\link{table}")} which has specialized
    \code{\link{format}} and \code{\link{print}} methods.  The
    \code{\link{factor}} method returns an integer vector.

    The matrix and data frame methods return a matrix of class
    \code{"\link{table}"}, obtained by applying \code{summary} to each
    column and collating the results.
}
\seealso{
  \code{\link{anova}},
  \code{\link{summary.glm}},
  \code{\link{summary.lm}}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.
}
\examples{
summary(attenu, digits = 4) #-> summary.data.frame(...), default precision
summary(attenu $ station, maxsum = 20) #-> summary.factor(...)

lst <- unclass(attenu$station) > 20 # logical with NAs
## summary.default() for logicals -- different from *.factor:
summary(lst)
summary(as.factor(lst))
}
\keyword{methods}
