% File src/library/base/man/pipeOp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{pipeOp}
\alias{|>}
\alias{pipeOp}
\title{Forward Pipe Operator}
\description{
  Pipe a value into a call expression or a function expression.
}
\usage{
\special{lhs |> rhs}
}
\arguments{
  \item{lhs}{expression producing a value.}
  \item{rhs}{a call expression or an expression of the form
    \code{symbol => call}.}
}
\details{
  A pipe expression passes, or pipes, the result of the left-hand side
  expression \code{lhs} to the right-hand side expression \code{rhs}.

  If the \code{rhs} expression is a call, then the \code{lhs} is
  inserted as the first argument in the call. So \code{x |> f(y)} is
  interpreted as \code{f(x, y)}.

  To avoid ambiguities, functions in \code{rhs} calls may not be
  syntactically special, such as \code{+} or \code{if}.
  
  Pipe notation allows a nested sequence of calls to be written in a way
  that may make the sequence of processing steps easier to follow.

  Currently, pipe operations are implemented as syntax transformations.
  So an expression written as \code{x |> f(y)} is parsed as \code{f(x,
  y)}. It is worth emphasizing that while the code in a pipeline is
  written sequentially, regular R semantics for evaluation apply and
  so piped expressions will be evaluated only when first used in the
  \code{rhs} expression.
}
\value{
  Returns the result of evaluating the transformed expression.
}
\section{Background}{
  The forward pipe operator is motivated by the pipe introduced in the
  \CRANpkg{magrittr} package, but is more streamlined. It is similar to
  the pipe or pipeline operators introduced in other languages, including
  F#, Julia, and JavaScript.
}
\examples{
# simple uses:
mtcars |> head()                      # same as head(mtcars)
mtcars |> head(2)                     # same as head(mtcars, 2)
mtcars |> subset(cyl == 4) |> nrow()  # same as nrow(subset(mtcars, cyl == 4))

# passing the lhs into an argument other than the first:
mtcars |> subset(cyl == 4) |> (function(d) lm(mpg ~ disp, data = d))()

# the pipe operator is implemented as a syntax transformation:
quote(mtcars |> subset(cyl == 4) |> nrow())

# regular R evaluation semantics apply
stop() |> (function(...) {})() # stop() is not used on RHS so is not evaluated
}
\keyword{programming}
\keyword{data}
