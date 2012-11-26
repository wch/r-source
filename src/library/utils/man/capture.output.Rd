% File src/library/utils/man/capture.output.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{capture.output}
\alias{capture.output}
\title{Send Output to a Character String or File}
\description{
  Evaluates its arguments with the output being returned as a character
  string or sent to a file.  Related to \code{\link{sink}} in the same
  way that \code{\link{with}} is related to \code{\link{attach}}.
}
\usage{
capture.output(\dots, file = NULL, append = FALSE)
}
\arguments{
  \item{\dots}{Expressions to be evaluated.}
  \item{file}{A file name or a \link{connection}, or \code{NULL} to return
    the output as a character vector.  If the connection is not open,
    it will be opened initially and closed on exit.}
  \item{append}{logical.  If \code{file} a file name or unopened
    connection, append or overwrite?}
}
\details{
  An attempt is made to write output as far as possible to \code{file}
  if there is an error in evaluating the expressions, but for
  \code{file = NULL} all output will be lost.
}
\value{
  A character string (if \code{file = NULL}), or invisible \code{NULL}.
}

\seealso{ \code{\link{sink}}, \code{\link{textConnection}} }

\examples{
require(stats)
glmout <- capture.output(example(glm))
glmout[1:5]
capture.output(1+1, 2+2)
capture.output({1+1; 2+2})
\dontrun{
## on Unix with enscript available
ps <- pipe("enscript -o tempout.ps","w")
capture.output(example(glm), file = ps)
close(ps)
}
}
\keyword{utilities}
