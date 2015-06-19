% File src/library/base/man/stopifnot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{stopifnot}
\alias{stopifnot}
\concept{assertion}
\title{Ensure the Truth of R Expressions}
\description{
  If any of the expressions in \code{\dots} are not \code{\link{all}}
  \code{TRUE}, \code{\link{stop}} is called, producing an error message
  indicating the \emph{first} of the elements of \code{\dots} which were
  not true.
}
\usage{
stopifnot(\dots)
}
\arguments{
  \item{\dots}{any number of (\code{\link{logical}}) \R expressions,
    which should evaluate to \code{\link{TRUE}}.}
}
\details{
  This function is intended for use in regression tests or also argument
  checking of functions, in particular to make them easier to read.

  \code{stopifnot(A, B)} is conceptually equivalent to
\preformatted{\{   if(any(is.na(A)) || !all(A)) stop(...) ;
      if(any(is.na(B)) || !all(B)) stop(...) \}
}
}
\value{
  (\code{\link{NULL}} if all statements in \code{\dots} are \code{TRUE}.)
}
\seealso{\code{\link{stop}}, \code{\link{warning}};
  \code{\link{assertCondition}} in package \pkg{tools} complements
  \code{stopifnot()} for testing warnings and errors.
}
\examples{
stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

m <- matrix(c(1,3,3,1), 2, 2)
stopifnot(m == t(m), diag(m) == rep(1, 2)) # all(.) |=>  TRUE

op <- options(error = expression(NULL))
# "disable stop(.)"  << Use with CARE! >>

stopifnot(all.equal(pi, 3.141593),  2 < 2, all(1:10 < 12), "a" < "b")
stopifnot(all.equal(pi, 3.1415927), 2 < 2, all(1:10 < 12), "a" < "b")

options(op)  # revert to previous error handler
}
\keyword{environment}
\keyword{programming}
\keyword{error}
