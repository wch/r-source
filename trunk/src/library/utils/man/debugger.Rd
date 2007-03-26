\name{debugger}
\alias{debugger}
\alias{dump.frames}
\title{Post-Mortem Debugging}
\description{
  Functions to dump the evaluation environments (frames) and to examine
  dumped frames.
}
\usage{
dump.frames(dumpto = "last.dump", to.file = FALSE)
debugger(dump = last.dump)
}
\arguments{
  \item{dumpto}{a character string. The name of the object or file to
    dump to.}
  \item{to.file}{logical. Should the dump be to an \R object or to a
    file?}
  \item{dump}{An \R dump object created by \code{dump.frames}.}
}
\details{
  To use post-mortem debugging, set the option \code{error} to be a call
  to \code{dump.frames}.  By default this dumps to an \R object
  \code{"last.dump"} in the workspace, but it can be set to dump to a
  file (a dump of the object produced by a call to \code{\link{save}}).
  The dumped object contain the call stack, the active environments and
  the last error message as returned by \code{\link{geterrmessage}}.

  When dumping to file, \code{dumpto} gives the name of the dumped
  object and the file name has \code{.rda} appended.

  A dump object of class \code{"dump.frames"} can be examined
  by calling \code{debugger}. This will give the error message and a
  list of environments from which to select repeatedly. When an
  environment is selected, it is copied and the \code{browser} called
  from within the copy.

  If \code{dump.frames} is installed as the error handler, execution
  will continue even in non-interactive sessions. See the examples for
  how to dump and then quit.
}
\value{
  Invisible \code{NULL}.
}
\note{
  Functions such as \code{\link{sys.parent}} and
  \code{\link{environment}} applied to closures will not work correctly
  inside \code{debugger}.

  If the error occurred when computing the default value of a formal argument
  the debugger will report "recursive default argument reference" when
  trying to examine that environment. 
  
  Of course post-mortem debugging will not work if \R is too damaged to
  produce and save the dump, for example if it has run out of workspace.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{options}} for setting \code{error} options;
  \code{\link{recover}} is an interactive debugger working similarly to
  \code{debugger} but directly after the error occurs.
}
\examples{
\dontrun{
options(error=quote(dump.frames("testdump", TRUE)))

f <- function() {
    g <- function() stop("test dump.frames")
    g()
}
f()   # will generate a dump on file "testdump.rda"
options(error=NULL)

## possibly in another R session
load("testdump.rda")
debugger(testdump)
Available environments had calls:
1: f()
2: g()
3: stop("test dump.frames")

Enter an environment number, or 0 to exit
Selection: 1
Browsing in the environment with call:
f()
Called from: debugger.look(ind)
Browse[1]> ls()
[1] "g"
Browse[1]> g
function() stop("test dump.frames")
<environment: 759818>
Browse[1]> 
Available environments had calls:
1: f()
2: g()
3: stop("test dump.frames")

Enter an environment number, or 0 to exit
Selection: 0

## A possible setting for non-interactive sessions
options(error=quote({dump.frames(to.file=TRUE); q()}))
}}
\keyword{utilities}
\keyword{error}
