\name{Control}
\title{Control Flow}
\usage{
if(cond) expr
if(cond) cons.expr  else  alt.expr
for(var in seq) expr
while(cond) expr
repeat expr
break
next
}
\alias{if}
\alias{else}
\alias{for}
\alias{while}
\alias{repeat}
\alias{break}
\alias{next}
\description{
These are the basic control-flow constructs of the \R language.
They function in much the same way as control statements
in any algol-like language.
}
\seealso{
\code{\link{ifelse}},
\code{\link{switch}}.
}
\examples{
for(i in 1:5) print(1:i)
}
\keyword{programming}
\keyword{iteration}
\keyword{logic}
