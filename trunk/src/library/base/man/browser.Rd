\name{browser}
\alias{browser}
\title{Environment Browser}
\description{
  Interrupt the execution of an expression and allow the inspection of
  the environment where \code{browser} was called from.
}
\usage{
browser()
}
\details{
  A call to \code{browser} can be included in the body of a function.
  When reached, this causes a pause in the execution of the
  current expression and allows access to the \R interpreter.

  At the browser prompt the user can enter commands or \R expressions.
  The commands are

  \describe{
    \item{\code{c}}{(or just return) exit the browser and continue
      execution at the next statement.}
    \item{\code{cont}}{synonym for \code{c}.}
    \item{\code{n}}{enter the step-through debugger. This changes the
      meaning of \code{c}: see the documentation for \code{\link{debug}}.}
    \item{\code{where}}{print a stack trace of all active function calls.}
    \item{\code{Q}}{exit the browser and the current evaluation and
      return to the top-level prompt.}
  }
  (Leading and trailing whitespace is ignored, except for return).

  Anything else entered at the browser prompt is interpreted as an
  \R expression to be evaluated in the calling environment: in
  particular typing an object name will cause the object to be printed,
  and \code{ls()} lists the objects in the calling frame.  (If you want
  to look at an object with a name such as \code{n}, print it
  explicitly.)

  The number of lines printed for the deparsed call can be limited by
  setting \code{\link{options}(deparse.max.lines)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{debug}}, and
  \code{\link{traceback}} for the stack on error.
}
\keyword{programming}
\keyword{environment}
