% File src/library/methods/man/nonStructure-class.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-12 R Core Team
% Distributed under GPL 2 or later

\name{nonStructure-class}
\docType{class}
\alias{nonStructure-class}
\alias{Math,nonStructure-method}
\alias{Math2,nonStructure-method}
\alias{Ops,vector,nonStructure-method}
\alias{Ops,nonStructure,vector-method}
\alias{Ops,nonStructure,nonStructure-method}

\title{A non-structure S4 Class for basic types }
\description{ S4 classes that are defined to extend one of the basic
  vector classes should contain the class
  \code{\linkS4class{structure}} if they behave like structures; that
  is, if they should retain their class behavior under math functions
  or operators, so long as their length is unchanged.
  On the other hand, if their class depends on the values in the
  object, not just its structure, then they should lose that class
  under any such transformations.  In the latter case, they should be
  defined to contain \code{nonStructure}.

 If neither of these strategies applies, the class likely needs some
 methods of its own for \code{\link{Ops}}, \code{\link{Math}}, and/or
 other generic functions. What is not usually a good idea is to allow
 such computations to drop down to the default, base code.  This is
 inconsistent with most definitions of such classes.}

\section{Methods}{
 Methods are defined for operators and math functions (groups
 \code{\link{Ops}}, \code{\link{Math}} and  \code{\link{Math2}}.  In
 all cases the result is an ordinary vector of the appropriate type.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.
}
\seealso{
         \code{\linkS4class{structure}}
}
\examples{
setClass("NumericNotStructure", contains = c("numeric","nonStructure"))
xx <- new("NumericNotStructure", 1:10)
xx + 1 # vector
log(xx) # vector
sample(xx) # vector
\dontshow{removeClass("NumericNotStructure")
}
}
\keyword{classes}
