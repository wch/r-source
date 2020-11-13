% File src/library/base/man/pipeOp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{pipeOp}
\alias{|>}
\alias{pipeOp}
\title{Pipe Operator}
\description{
  Pipe a value into a call expression or a function.
}
\usage{
\special{lhs |> rhs}
}
\arguments{
  \item{lhs}{expression producing a value.}
  \item{rhs}{a call expression, a \code{function} expression, or a
    function name.}
}
\details{
  A pipe expression passes, or pipes, the result of the \code{lhs}
  expression to the \code{rhs} expression.

  If the \code{rhs} expression is a function name or a function
  expression, then the function is called with the \code{lhs} value as
  its argument. So \code{x |> f} is interpreted as \code{f(x)}.

  If the \code{rhs} expression is a call, then the \code{lhs} is
  inserted as the first argument in the call. So \code{x |> f(y)} is
  interpreted as \code{f(x, y)}. To avoid ambiguities, functions in
  \code{lhs} calls may not be syntactically special, such as \code{+} or
  \code{if}.
  
  Pipe notation allows a nested sequence of calls to be written in a way
  that may make the sequence of processing steps easier to follow.

  Currently, pipe operations are implemented as syntax transformations.
  So an expression written as \code{x |> f(y)} is parsed as \code{f(x,
  y)}.
}
\value{
  Returns the result of evaluating the transformed expression.
}
\note{ \bold{Pipe support is experimental and may change prior to release.} }
\seealso{
  The pipe operator is motivated by the pipe introduced in the
  \CRANpkg{magrittr} package, but more streamlined. It is similar to
  pipe or pipeline operators introduced in other languages, including
  Julia and JavaScript.
}
\examples{
# simple uses:
mtcars |> head
mtcars |> head(2)
mtcars |> subset(cyl == 4) |> nrow

# passing the lhs into an argument other than the first:
mtcars |> subset(cyl == 4) |> function(d) lm(mpg ~ disp, data = d)
mtcars |> subset(cyl == 4) |> \(d) lm(mpg ~ disp, data = d)

# the pipe operator is implemented as a syntax transformation:
quote(mtcars |> subset(cyl == 4) |> nrow)
}
\keyword{data}
