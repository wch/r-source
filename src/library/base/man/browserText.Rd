\name{browserText}
\Rdversion{1.1}
\alias{browserText}
\alias{browserCondition}
\title{
Functions to retrieve values supplied by calls to browser.
}
\description{
A call to browser can provide context by supplying either a text
argument or a condition argument.  These functions can be used to
retrieve either of these arguments.
}
\usage{
browserText()
browserCondition()
}
\details{
The innermost browser context is queried for values of the 
supplied text if \code{browserText} is called
and the supplied condition if \code{browserCondition} is called.
}
\value{
\code{browserText} returns the text, while \code{browserCondition}
returns the condition.
}
\author{
R. Gentleman
}
\note{
It may be of interest to allow for querying further up the set of browser
contexts and this functionality may be added at a later date.
}

\seealso{
\code{\link{browser}}
}
\keyword{debugging}
