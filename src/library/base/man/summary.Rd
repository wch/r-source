\name{summary}
\alias{summary}
\alias{summary.default}
\alias{summary.data.frame}
\alias{summary.factor}
\alias{summary.matrix}
\title{Object Summaries}
\description{
  \code{summary} is a generic function used to produce result summaries
  of the results of various model fitting functions.  The function
  invokes particular \code{\link{methods}} which depend on the
  \code{\link[base]{class}} of the first argument.
}
\usage{
summary(object, \dots)

\method{summary}{default}(object, \dots, digits = max(3, getOption("digits")-3))
\method{summary}{data.frame}(object, maxsum = 7,
       digits = max(3, getOption("digits")-3), \dots)
\method{summary}{factor}(object, maxsum = 100, \dots)
\method{summary}{matrix}(object, \dots)
}
\arguments{
  \item{object}{an object for which a summary is desired.}
  \item{maxsum}{integer, indicating how many levels should be shown for
    \code{\link{factor}}s.}
  \item{digits}{integer, used for number formatting with
    \code{\link{signif}()} (for \code{summary.default}) or
    \code{\link{format}()} (for \code{summary.data.frame}).}
  \item{\dots}{additional arguments affecting the summary produced.}
}
\details{
    For \code{\link{factor}}s, the frequency of the first \code{maxsum - 1}
    most frequent levels is shown, where the less frequent levels are
    summarized in \code{"(Others)"} (resulting in \code{maxsum} frequencies).

    The functions \code{summary.lm} and \code{summary.glm} are examples
    of particular methods which summarise the results produced by
    \code{\link{lm}} and \code{\link{glm}}.
}
\value{
    The form of the value returned by \code{summary} depends on the
    class of its argument.  See the documentation of the particular
    methods for details of what is produced by that method.
}
\seealso{
  \code{\link{anova}},
  \code{\link{summary.glm}},
  \code{\link{summary.lm}}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.
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
