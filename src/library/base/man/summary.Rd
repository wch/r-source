\name{summary}
\title{Object Summaries}
\usage{
summary(object, \dots)
}
\alias{summary}
\arguments{
\item{object}{an object for which a summary is desired.}
\item{\dots}{additional arguments affecting the summary
produced.}
}
\description{
\code{summary} is a generic function used
to produce result summaries of the results of
various model fitting functions.
The function invokes particular \emph{methods}
which depend on the \emph{class} of the first argument.

The functions \code{summary.lm} and \code{summary.glm}
are examples of particular methods which summarise
the results produced by \code{lm} and \code{glm}.
}
\value{
The form of the value returned by \code{summary} depends on the
class of its argument.  See the documentation of the
particular methods for details of what is produced by that method.
}
\seealso{
\code{\link{anova}},
\code{\link{summary.glm}},
\code{\link{summary.lm}}.
}
\keyword{methods}
