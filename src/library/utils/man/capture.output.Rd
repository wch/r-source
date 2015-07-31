% File src/library/utils/man/capture.output.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
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
capture.output(\dots, file = NULL, append = FALSE,
               type = c("output", "message"), split = FALSE)
}
\arguments{
  \item{\dots}{Expressions to be evaluated.}
  \item{file}{A file name or a \link{connection}, or \code{NULL} to return
    the output as a character vector.  If the connection is not open,
    it will be opened initially and closed on exit.}
  \item{append}{logical.  If \code{file} a file name or unopened
    connection, append or overwrite?}
  \item{type, split}{are passed to \code{\link{sink}(), see there.}
}
\details{
  An attempt is made to write output as far as possible to \code{file}
  if there is an error in evaluating the expressions, but for
  \code{file = NULL} all output will be lost.

  Messages sent to \code{\link{stderr}()} (including those from
  \code{\link{message}}, \code{\link{warning}} and \code{\link{stop}})
  are captured by \code{type = "message"}.
}
\value{
  A character string (if \code{file = NULL}), or invisible \code{NULL}.
}

\seealso{ \code{\link{sink}}, \code{\link{textConnection}} }

\examples{
require(stats)
glmout <- capture.output(summary(glm(case ~ spontaneous+induced,
                                     data = infert, family = binomial())))
glmout[1:5]
capture.output(1+1, 2+2)
capture.output({1+1; 2+2})

\dontrun{## on Unix-alike with a2ps available%% ?? pandoc with obeylines, obeyspaces
op <- options(useFancyQuotes=FALSE)
pdf <- pipe("a2ps -o - | ps2pdf - tempout.pdf", "w")
capture.output(example(glm), file = pdf)
close(pdf); options(op) ; system("evince tempout.pdf &")
}% dont
}
\keyword{utilities}
