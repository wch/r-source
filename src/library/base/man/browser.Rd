\name{browser}
\title{Environment Browser}
\usage{
browser()
}
\alias{browser}
\value{
A call to \code{browser} causes a pause in the execution
of the current expression and runs a copy of the \R interpreter
which has access to variables local to the environment
where the call took place.
Local variables can be listed with \code{ls},
and manipulated with \R expressions typed to this sub-interpreter.
The interpreter copy is exited by typing \code{c}.
Execution then resumes at the
statement following the call to
\code{browser}.

Typing \code{n} causes the step-through-debugger, to start
and it is possible to step through the remainder of
the function one line at a time.

Typing \code{Q} quits the current execution and returns you to the
top--level prompt.
}
\seealso{
\code{\link{debug}}, and \code{\link{traceback}} for the stack an error.
}
\keyword{programming}
\keyword{environment}
