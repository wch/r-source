\name{assignOps}
\alias{<-}
\alias{=}
\alias{<<-}
\alias{->}
\alias{->>}
\title{Assignment Operators}
\description{
  Assign a value to a name.
}
\usage{
x <- value
x <<- value
value -> x
value ->> x

x = value
}
\arguments{
  \item{x}{a variable name (possibly quoted).}
  \item{value}{a value to be assigned to \code{x}.}
}
\details{
  There are three different assignment operators: two of them
  have leftwards and rightwards forms.

  The operators \code{<-} and \code{=} assign into the environment in
  which they are evaluated.  The  operator\code{<-} can be used
  anywhere, whereas the operator \code{=} is only allowed at the top
  level (e.g., in the complete expression typed at the command prompt)
  or as one of the subexpressions in a braced list of expressions.

  The operators \code{<<-} and \code{->>} cause a search to made through
  the environment for an existing definition of the variable being
  assigned.  If such a variable is found (and its binding is not locked)
  then its value is redefined, otherwise assignment takes place in the
  global environment.  Note that their semantics differ from that in the
  S language, but are useful in conjunction with the scoping rules of
  \R.  See \sQuote{The R Language Definition} manual for further details
  and examples.

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
\note{
  Prior to \R 2.4.0 the base environment was ignored in the search for a
  binding by \code{<<-} and \code{->>}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chamber, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer (for \code{=}).
}

\seealso{
  \code{\link{assign}},
  \code{\link{environment}}.
}
\keyword{data}
