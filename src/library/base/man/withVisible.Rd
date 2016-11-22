% File src/library/utils/man/withVisible.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{withVisible}
\alias{withVisible}
\title{Return both a Value and its Visibility}
\description{
This function evaluates an expression, returning it in a two element list
containing its value and a flag showing whether it would automatically print.
}
\usage{
withVisible(x)
}
\arguments{
  \item{x}{an expression to be evaluated.}
}
\details{
  The argument, \emph{not} an \code{\link{expression}} object, rather
  an (unevaluated function) \code{\link{call}}, is evaluated in the
  caller's context.

  This is a \link{primitive} function.
}
\value{
  \item{value }{The value of \code{x} after evaluation.}
  \item{visible }{logical; whether the value would auto-print.}
}
\seealso{\code{\link{invisible}}, \code{\link{eval}};
  \code{\link{withAutoprint}()} calls \code{\link{source}()} which
  itself uses \code{withVisible()} in order to correctly
  \dQuote{auto print}.
}
\examples{
x <- 1
withVisible(x <- 1) # *$visible is FALSE
x
withVisible(x)      # *$visible is TRUE

# Wrap the call in evalq() for special handling

df <- data.frame(a = 1:5, b = 1:5)
evalq(withVisible(a + b), envir = df)
}
\keyword{programming}
