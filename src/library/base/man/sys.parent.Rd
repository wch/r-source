\name{sys.parent}
\title{Functions to access the function call stack.}
\usage{
sys.call(which=<<see below>>)
sys.frame(which=<<see below>>)
sys.nframe()
sys.function(n=<<see below>>)
sys.parent(n=1)

sys.calls()
sys.frames()
sys.parents()
sys.on.exit()
sys.status()
parent.frame(n=1)
}
\alias{sys.parent}
\alias{sys.call}
\alias{sys.calls}
\alias{sys.frame}
\alias{sys.frames}
\alias{sys.nframe}
\alias{sys.function}
\alias{sys.parents}
\alias{sys.on.exit}
\alias{sys.status}
\alias{parent.frame}
\arguments{
\item{which}{the frame number.}
\item{n}{the number of frame generations to go back.}
}
\description{
These functions provide access to \code{\link{environment}}s (``frames''
in S-speak) associated with functions further up the calling stack.
}
\details{
\code{\link{.GlobalEnv}}. 
is given number 1 in the list of frames. Each subsequent function
evaluation increases the frame stack by 1 and the environment for evaluation
of that function is returned by \code{sys.frame} with the appropriate index.

The parent of a function evaluation is the environment in which the
function was called. It is not necessarily one less than the frame
number of the current evaluation, nor the environment from which it is
called.  \code{sys.parent} return the number of the parent frame if n
is 1, the grandparent if n is 2, and so forth. \code{sys.frame}
returns the environment associated with a given frame number.

\code{sys.call} and \code{sys.frame} both accept either positive or negative
values for the argument \code{which}.  Positive values of \code{which} count up
from frame 1 and negative values are count back from frame n.

\code{parent.frame(n)} is a convenient shorthand for sys.frame(sys.parent(n)) 

Notice that even though the \code{sys.\emph{xxx} functions are
interpreted, their contexts are not counted nor are they reported.  There
is no access to them.

\code{sys.status()} simply returns a list with components
\code{sys.calls}, \code{sys.parents}, \code{sys.frames}.
}
\seealso{
  \code{\link{eval}} for the usage of \code{sys.frame}.
}
\examples{
ff <- function(x) gg(x)
gg <- function(y) sys.status()
str(ff(1))
}
\keyword{programming}
\keyword{data}
