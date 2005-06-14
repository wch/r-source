\name{Syntax}
\alias{Syntax}
\title{Operator Syntax}
\description{
  Outlines \R syntax and gives the precedence of operators
}
\details{
  The following unary and binary operators are defined.  They are listed
  in precedence groups, from highest to lowest.
  \tabular{ll}{
    \code{[ [[}\tab indexing\cr
    \code{::}\tab name space/variable name separator\cr
    \code{$ @}\tab component / slot extraction\cr
    \code{^}\tab exponentiation (right to left)\cr
    \code{- +}\tab unary minus and plus\cr
    \code{:}\tab sequence operator\cr
    \code{\%any\%}\tab special operators\cr
    \code{* /}\tab multiply, divide\cr
    \code{+ -}\tab (binary) add, subtract\cr
    \code{< > <= >= == !=}\tab ordering and comparison\cr
    \code{!}\tab negation\cr
    \code{&  &&}\tab and\cr
    \code{| ||}\tab or\cr
    \code{~}\tab as in formulae\cr
    \code{-> ->>}\tab rightwards assignment\cr
    \code{=}\tab assignment (right to left)\cr
    \code{<- <<-}\tab assignment (right to left)\cr
    \code{?}\tab help (unary and binary)\cr
  }
  Within an expression operators of equal precedence are evaluated
  from left to right except where indicated.

  The links in the \bold{See Also} section covers most other aspects of
  the basic syntax.
}
\note{
  There are substantial precedence differences between \R and S.  In
  particular, in S \code{?} has the same precedence as \code{+ -} and
  \code{& && | ||} have equal precedence.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{Arithmetic}}, \code{\link{Comparison}}, \code{\link{Control}},
  \code{\link{Extract}}, \code{\link{Logic}}, \code{\link{Paren}},
  \code{\link{Quotes}}

  The \emph{R Language Definition} manual.
}
\keyword{documentation}
