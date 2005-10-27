\name{call}
\alias{call}
\alias{is.call}
\alias{as.call}
\title{Function Calls}
\description{
  Create or test for objects of mode \code{"call"}.
}
\usage{
call(name, \dots)
is.call(x)
as.call(x)
}
\arguments{
  \item{name}{a character string naming the function to be called.}
  \item{\dots}{arguments to be part of the call.}
  \item{x}{an arbitrary \R object.}
}
\details{
  \code{call} returns an unevaluated function call, that is, an
  unevaluated expression which consists of the named function applied to
  the given arguments (\code{name} must be a quoted string which gives
  the name of a function to be called).

  \code{call} is a primitive, so the first argument (named or not) is
  taken as \code{name} and the remaining arguments as arguments for the
  constructed call: \code{call(x="c", 1,3, name="foo")} is a call to
  \code{c} and not to \code{foo}.
  
  \code{is.call} is used to determine whether \code{x} is a call (i.e.,
  of mode \code{"call"}).
  %NO We don't differentiate between expressions and function calls.
  %NO So \code{is.call} is the same as \code{is.expression}.
  It is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
  
  Objects of mode \code{"list"} can be coerced to mode \code{"call"}.
  The first element of the list becomes the function part of the call,
  so should be a function or the name of one (as a symbol; a quoted
  string will not do).  
}
\seealso{
  \code{\link{do.call}} for calling a function by name and argument
  list;
  \code{\link{Recall}} for recursive calling of functions;
  further
  \code{\link{is.language}},
  \code{\link{expression}},
  \code{\link{function}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
is.call(call) #-> FALSE: Functions are NOT calls

# set up a function call to round with argument 10.5
cl <- call("round", 10.5)
is.call(cl)# TRUE
cl
# such a call can also be evaluated.
eval(cl)# [1] 10
}
\keyword{programming}
\keyword{attribute}
