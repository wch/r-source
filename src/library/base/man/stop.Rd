\name{stop}
\title{Stop Function Execution}
\usage{
stop(\dots, call. = TRUE)
geterrmessage()
}
\alias{stop}
\alias{geterrmessage}
\arguments{
  \item{\dots}{character vectors (which are pasted together with no
    separator) or \code{NULL}.}
  \item{call.}{logical, indicating if the call should become part of the
    error message.}
}
\description{
  \code{stop} stops execution of the current expression, prints the
  message given as its argument, then executes an error action.

  \code{geterrmessage} gives the last error message.
}
\details{
  The error action is controlled by the current error handler set by
  \code{options(error=)}. The default behaviour  (the \code{NULL}
  error-handler) in interactive use is
  to return to the top level prompt, and in non-interactive use to
  (effectively) call \code{\link{q}("no", status=1, runLast=FALSE}).

  Errors will be truncated to \code{getOption("warning.length")}
  characters, default 1000.
}
\value{
  \code{geterrmessage} gives the last error message, as character string
  ending in \code{"\n"}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{warning}}, \code{\link{try}} to catch errors and retry,
  and \code{\link{options}} for setting error handlers.
  \code{\link{stopifnot}} for validity testing.
}
\examples{
options(error = expression(NULL))# don't stop on stop(.)  << Use with CARE! >>

iter <- 12
if(iter > 10) stop("too many iterations")

tst1 <- function(...) stop("dummy error")
tst1(1:10,long,calling,expression)

tst2 <- function(...) stop("dummy error", call. = FALSE)
tst2(1:10,long,calling,expression,but.not.seen.in.Error)

options(error = NULL)# revert to default
}
\keyword{environment}
\keyword{programming}
\keyword{error}
