\name{call}
\title{Function Calls}
\usage{
call(name, \dots)

is.call(expr)
as.call(expr)
}
\alias{call}
\alias{is.call}
\alias{as.call}
\value{
An unevaluated function call, that is, an unevaluated expression
which consists of the named function applied to the given arguments
(\code{name} must be a quoted string which gives
the name of a function to be called).

\code{is.call} is used to determine whether \code{expr} is a call.
%NO We don't differentiate between expressions and function calls.
%NO So \code{is.call} is the same as \code{is.expression}.

It is not possible to coerce objects to mode call
(objects either are calls or they are not calls).
\code{as.call} returns its argument if it is a call and
otherwise terminates with an error message.
}
\seealso{
  \code{\link{do.call}} for calling a function by name and argument
  list; \code{\link{is.language}}, \code{\link{expression}}, \code{\link{function}}.
}
\examples{
is.call(call) #-> FALSE: Functions are NOT calls

# set up a function call to round with argument 10.5
cl <- call("round",10.5)
is.call(cl)# TRUE
cl
# such a call can also be evaluated.
eval(cl)# [1] 10
}
\keyword{programming}
\keyword{attribute}
