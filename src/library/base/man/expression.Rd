\name{expression}
\title{Unevaluated Expressions}
\usage{
expression(\dots)

is.expression(expr)
as.expression(expr)
}
\alias{expression}
\alias{is.expression}
\alias{as.expression}
\value{
An object with class ``expression'' containing its
unevaluated arguments.  Expression objects are
lists of ``calls'' and so the subsetting operations which apply
to lists also apply to them.  For compatibility with S,
the \code{mode} function will return \code{"expression"}
for expression objects.

\code{is.expression} returns \code{TRUE} if expr is
an expression object and \code{FALSE} otherwise.

\code{as.expression} attempts to coerce its argument
into an expression object.
}
\seealso{
\code{\link{call}}, \code{\link{eval}}, \code{\link{function}}.
}
\examples{
length(ex1 <- expression(1+ 0:9))# 1
ex1
eval(ex1)# 1:10
}
