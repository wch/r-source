\name{summary}
\title{Object Summaries}
\usage{
summary(object, \dots)

summary.default   (object, \dots, digits = max(3, getOption("digits") -3))
summary.data.frame(object, maxsum = 7, \dots)
summary.factor    (object, maxsum = 100, \dots)
summary.matrix    (object, \dots)
}
\alias{summary}
\alias{summary.default}
\alias{summary.data.frame}
\alias{summary.factor}
\alias{summary.matrix}
\arguments{
\item{object}{an object for which a summary is desired.}
\item{maxsum}{integer, indicating how many levels should be shown for
  \code{\link{factor}}s.}
\item{\dots}{additional arguments affecting the summary produced.}
}
\description{
    \code{summary} is a generic function used to produce result
    summaries of the results of various model fitting functions.  The
    function invokes particular \code{\link{methods}} which depend on
    the \code{\link{class}} of the first argument. 
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
\seealso{\code{\link{anova}},\code{\link{summary.glm}},\code{\link{summary.lm}}.
}
\examples{
options(digits=5)
data(attenu)
summary(attenu) #-> summary.data.frame(..)
summary(attenu $ station, maxsum = 20) #-> summary.factor(..)
}
\keyword{methods}
