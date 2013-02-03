% File src/library/base/man/Syntax.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

% = is treated specially in the parser: see PR#14595

\name{Syntax}
\alias{Syntax}
\title{Operator Syntax and Precedence}
\description{
  Outlines \R syntax and gives the precedence of operators.
}
\details{
  The following unary and binary operators are defined.  They are listed
  in precedence groups, from highest to lowest.
  \tabular{ll}{
    \code{:: :::}\tab access variables in a namespace\cr
    \code{$ @}\tab component / slot extraction\cr
    \code{[ [[}\tab indexing\cr
    \code{^}\tab exponentiation (right to left)\cr
    \code{- +}\tab unary minus and plus\cr
    \code{:}\tab sequence operator\cr
    \code{\%any\%}\tab special operators (including \code{\%\%} and \code{\%/\%})\cr
    \code{* /}\tab multiply, divide\cr
    \code{+ -}\tab (binary) add, subtract\cr
    \code{< > <= >= == !=}\tab ordering and comparison\cr
    \code{!}\tab negation\cr
    \code{&  &&}\tab and\cr
    \code{| ||}\tab or\cr
    \code{~}\tab as in formulae\cr
    \code{-> ->>}\tab rightwards assignment\cr
    \code{<- <<-}\tab assignment (right to left)\cr
    \code{=}\tab assignment (right to left)\cr
    \code{?}\tab help (unary and binary)\cr
  }
  Within an expression operators of equal precedence are evaluated
  from left to right except where indicated.  (Note that \code{=} is not
  necessarily an operator.)

  The binary operators \code{::}, \code{:::}, \code{$} and \code{@} require
  names or string constants on the right hand side, and the first two
  also require them on the left.

  The links in the \bold{See Also} section cover most other aspects of
  the basic syntax.
}
\note{
  There are substantial precedence differences between \R and S.  In
  particular, in S \code{?} has the same precedence as (binary) \code{+ -}
  and \code{& && | ||} have equal precedence.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{Arithmetic}}, \code{\link{Comparison}}, \code{\link{Control}},
  \code{\link{Extract}}, \code{\link{Logic}},
  \code{\link{NumericConstants}}, \code{\link{Paren}},
  \code{\link{Quotes}}, \code{\link{Reserved}}.

  The \sQuote{R Language Definition} manual.
}
\keyword{documentation}
\keyword{programming}
