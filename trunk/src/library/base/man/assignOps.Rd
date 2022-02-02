% File src/library/base/man/assignOps.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{assignOps}
\alias{<-}
\alias{=}
\alias{<<-}
\alias{->}
\alias{->>}
\alias{assignOps}
\title{Assignment Operators}
\description{
  Assign a value to a name.
}
\usage{
\special{x <- value}
\special{x <<- value}
\special{value -> x}
\special{value ->> x}

\special{x = value}
}
\arguments{
  \item{x}{a variable name (possibly quoted).}
  \item{value}{a value to be assigned to \code{x}.}
}
\details{
  There are three different assignment operators: two of them
  have leftwards and rightwards forms.

  The operators \code{<-} and \code{=} assign into the environment in
  which they are evaluated.  The  operator \code{<-} can be used
  anywhere, whereas the operator \code{=} is only allowed at the top
  level (e.g., in the complete expression typed at the command prompt)
  or as one of the subexpressions in a braced list of expressions.

  The operators \code{<<-} and \code{->>} are normally only used in
  functions, and cause a search to be made through parent environments
  for an existing definition of the variable being assigned.  If such
  a variable is found (and its binding is not locked) then its value
  is redefined, otherwise assignment takes place in the global
  environment.  Note that their semantics differ from that in the S
  language, but are useful in conjunction with the scoping rules of
  \R.  See \sQuote{The R Language Definition} manual for further
  details and examples.

  In all the assignment operator expressions, \code{x} can be a name
  or an expression defining a part of an object to be replaced (e.g.,
  \code{z[[1]]}).  A syntactic name does not need to be quoted,
  though it can be (preferably by \link{backtick}s).

  The leftwards forms of assignment \code{<- = <<-} group right to left,
  the other from left to right.
}
\value{
  \code{value}.  Thus one can use \code{a <- b <- c <- 6}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer (for \code{=}).
}

\seealso{
  \code{\link{assign}} (and its inverse \code{\link{get}}),
  for \dQuote{subassignment} such as \code{x[i] <- v},
  see \code{\link{[<-}}; % ./Extract.Rd
  further, \code{\link{environment}}.
}
\keyword{data}
