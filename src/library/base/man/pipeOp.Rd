% File src/library/base/man/pipeOp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
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
  \item{rhs}{a call expression.}% or an expression of the form \code{symbol => call}
}
\details{
  A pipe expression passes, or \sQuote{pipes}, the result of the left-hand-side
  expression \code{lhs} to the right-hand-side expression \code{rhs}.

  The \code{lhs} is
  inserted as the first argument in the call. So \code{x |> f(y)} is
  interpreted as \code{f(x, y)}.

  To avoid ambiguities, functions in \code{rhs} calls may not be
  syntactically special, such as \code{+} or \code{if}.
  
  It is also possible to use a named argument with the placeholder
  \code{_} in the \code{rhs} call to specify where the \code{lhs} is to
  be inserted.  The placeholder can only appear once on the \code{rhs}.
  
  The placeholder can also be used as the first argument in an
  extraction call, such as \code{_$coef}. More generally, it can be used
  as the head of a chain of extractions, such as \code{_$coef[[2]]},
  using a sequence of the extraction functions \code{$}, \code{[},
  \code{[[}, or \code{@}.

  Pipe notation allows a nested sequence of calls to be written in a way
  that may make the sequence of processing steps easier to follow.

  Currently, pipe operations are implemented as syntax transformations.
  So an expression written as \code{x |> f(y)} is parsed as \code{f(x,
  y)}.  It is worth emphasizing that while the code in a pipeline is
  written sequentially, regular R semantics for evaluation apply and
  so piped expressions will be evaluated only when first used in the
  \code{rhs} expression.
}
\value{
  Returns the result of evaluating the transformed expression.
}
\section{Background}{
  The forward pipe operator is motivated by the pipe introduced in the
  \CRANpkg{magrittr} package, but is more streamlined.  It is similar to
  the pipe or pipeline operators introduced in other languages, including
  F#, Julia, and JavaScript.
}
\section{Warning}{
  This was introduced in \R 4.1.0.  Code using it will not be parsed
  as intended (probably with an error) in earlier versions of \R.
}
\examples{
# simple uses:
mtcars |> head()                      # same as head(mtcars)
mtcars |> head(2)                     # same as head(mtcars, 2)
mtcars |> subset(cyl == 4) |> nrow()  # same as nrow(subset(mtcars, cyl == 4))

# to pass the lhs into an argument other than the first, either
# use the _ placeholder with a named argument:
mtcars |> subset(cyl == 4) |> lm(mpg ~ disp, data = _)
# or use an anonymous function:
mtcars |> subset(cyl == 4) |> (function(d) lm(mpg ~ disp, data = d))()
mtcars |> subset(cyl == 4) |> (\(d) lm(mpg ~ disp, data = d))()
# or explicitly name the argument(s) before the "one":
mtcars |> subset(cyl == 4) |> lm(formula = mpg ~ disp)

# using the placeholder as the head of an extraction chain:
mtcars |> subset(cyl == 4) |> lm(formula = mpg ~ disp) |> _$coef[[2]]

# the pipe operator is implemented as a syntax transformation:
quote(mtcars |> subset(cyl == 4) |> nrow())

# regular R evaluation semantics apply
stop() |> (function(...) {})() # stop() is not used on RHS so is not evaluated
}
\keyword{programming}
\keyword{data}
