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
\arguments{
\item{which}{the frame number.}
\item{n}{the number of frame generations to go back.}
}
\description{
These functions provide access to \code{\link{environment}}s (``frames''
in S-speak) associated with functions further up the calling stack.
}
\details{
You need access to two different types of environments.
\enumerate{
    \item{You need access to the environment where the arguments to a function
	are defined; this is what \code{sys.parent} does.}
    \item{You also need access to the environment where a function is
	being evaluated; this is what \code{sys.frame} does.}
}
Often \code{sys.parent()} is the top-level environment,
\code{\link{.GlobalEnv}}. 
This is given number 1 in the list of frames. Each subsequent function
evaluation increases the frame stack by 1 and the environment for evaluation
of that function is returned by \code{sys.frame} with the appropriate index.

\code{sys.call} and \code{sys.frame} both accept either positive or negative
values for the argument \code{which}.  Positive values of \code{which} count up
from frame 1 and negative values are count back from frame n.

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
