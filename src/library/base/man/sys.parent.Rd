\name{sys.parent}
\title{Functions to Access the Function Call Stack}
\usage{
sys.call(which = 0)
sys.frame(which = 0)
sys.nframe()
sys.function(n = 0)
sys.parent(n = 1)

sys.calls()
sys.frames()
sys.parents()
sys.on.exit()
sys.status()
parent.frame(n = 1)
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
\item{which}{the frame number if non-negative, the number of generations
  to go back if negative. (See the Details section.)}
\item{n}{the number of frame generations to go back.}
}
\description{
  These functions provide access to \code{\link{environment}}s (``frames''
  in S terminology) associated with functions further up the calling stack.
}
\details{
  \code{\link{.GlobalEnv}} is given number 0 in the list of frames.
  Each subsequent function evaluation increases the frame stack by 1
  and the environment for evaluation of that function is returned by
  \code{sys.frame} with the appropriate index.

  The parent of a function evaluation is the environment in which the
  function was called. It is not necessarily numbered one less than the frame
  number of the current evaluation, nor is it the environment within
  which the function was defined.  \code{sys.parent} returns the number
  of the parent frame if \code{n} is 1 (the default), the grandparent if
  \code{n} is 2, and so on. \code{sys.frame} returns the environment
  associated with a given frame number.

  \code{sys.call} and \code{sys.frame} both accept integer values
  for the argument \code{which}.  Non-negative values of
  \code{which} are normal frame numbers whereas negative values are counted
  back from the frame number of the current evaluation.

  \code{sys.nframe} returns the number of the current frame in that
  list. \code{sys.function} gives the definition of the function
  curently being evaluated in the frame \code{n} generations back.
  
  \code{sys.frames} gives a list of all the active frames and
  \code{sys.parents} gives the indices of the parent frames of each
  of the frames.
  
  Notice that even though the \code{sys.}\emph{xxx} functions (except
  \code{sys.status}) are
  interpreted, their contexts are not counted nor are they reported.
  There is no access to them.

  \code{sys.status()} returns a list with components \code{sys.calls},
  \code{sys.parents} and \code{sys.frames}.

  \code{sys.on.exit()} retrieves the expression stored for use by
  \code{\link{on.exit}} in the function currently being evaluated.
  (Note that this differs from S, which returns a list of expressions
  for the current frame and its parents.)

  \code{parent.frame(n)} is a convenient shorthand for
  \code{sys.frame(sys.parent(n))} (implemented slightly more efficiently).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (not \code{parent.frame}.)
}
\seealso{
  \code{\link{eval}} for the usage of \code{sys.frame} and \code{parent.frame}.
}
\examples{
ff <- function(x) gg(x)
gg <- function(y) sys.status()
str(ff(1))

gg <- function(y) {
    ggg <- function() {
        cat("current frame is", sys.nframe(), "\n")
        cat("parents are", sys.parents(), "\n")
        print(sys.function(0)) # ggg
        print(sys.function(2)) # gg
    }
    if(y > 0) gg(y-1) else ggg()
}
gg(3)

t1 <- function() {
  aa <- "here"
  t2 <- function() {
    ## in frame 2 here
    cat("current frame is", sys.nframe(), "\n")
    str(sys.calls()) ## list with two components t1() and t2()
    cat("parents are frame nos", sys.parents(), "\n") ## 0 1
    print(ls(envir=sys.frame(-1))) ## [1] "aa" "t2"
    invisible()
  }
  t2()
}
t1()

test.sys.on.exit <- function() {
  on.exit(print(1))
  ex <- sys.on.exit()
  str(ex)
  cat("exiting...\n")
}
test.sys.on.exit()
## gives `language print(1)', prints 1 on exit
}
\keyword{programming}
\keyword{data}
