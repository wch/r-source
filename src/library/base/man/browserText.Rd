% File src/library/base/man/browserText.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{browserText}
\alias{browserText}
\alias{browserCondition}
\alias{browserSetDebug}
\title{
Functions to Retrieve Values Supplied by Calls to the Browser
}
\description{
A call to browser can provide context by supplying either a text
argument or a condition argument.  These functions can be used to
retrieve either of these arguments.
}
\usage{
browserText(n = 1)
browserCondition(n = 1)
browserSetDebug(n = 1)
}
\arguments{
  \item{n}{The number of contexts to skip over, it must be non-negative.}
}
\details{
Each call to \code{browser} can supply either a text string or a condition.
The functions \code{browserText} and \code{browserCondition} provide ways
to retrieve those values.  Since there can be multiple browser contexts
active at any time we also support retrieving values from the different
contexts.  The innermost (most recently initiated) browser context is
numbered 1: other contexts are numbered sequentially.

\code{browserSetDebug} provides a mechanism for initiating the browser in
one of the calling functions.  See \code{\link{sys.frame}} for a more
complete discussion of the calling stack. To use \code{browserSetDebug}
you select some calling function, determine how far back it is in the call
stack and call \code{browserSetDebug} with \code{n} set to that value.
Then, by typing \code{c} at the browser prompt you will cause evaluation
to continue, and provided there are no intervening calls to browser or
other interrupts, control will halt again once evaluation has returned to
the closure specified.  This is similar to the up functionality in gdb
or the "step out" functionality in other debuggers.
}
\value{
\code{browserText} returns the text, while \code{browserCondition}
returns the condition from the specified browser context.

\code{browserSetDebug} returns NULL, invisibly.
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
