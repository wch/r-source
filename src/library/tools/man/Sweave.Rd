\name{Sweave}
\alias{Sweave}
\alias{Stangle}
\title{Automatic Generation of Reports}
\description{
  \code{Sweave} provides a flexible framework for mixing text and S code
  for automatic report generation. The basic idea is to replace the S
  code with it's output, such that the final document only contains the
  text and the output of the statistical anlysis. 
}
\usage{
Sweave(file, driver=RWeaveLatex(), ...)
Stangle(file, driver=RTangle(), ...)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{driver}{The actual workhorse, see details below.}
  \item{\dots}{Further arguments passed to the driver's setup function.}
}
\details{
  Automatic generation of reports by mixing word processing markup (like
  latex) and S code. The S code gets replaced by it's output (text or
  graphs) in the final markup file. This allows to re-generate a report
  if the input data change and documents the code to reproduce the
  analysis in the same file that also produces the report.

  \code{Sweave} combines the documentation and code chunks together
  (or their output) into a single document. \code{Stangle} extracts only
  the code from the noweb file creating a valid S source file (that can
  be run using \code{\link{source}}). Code inside \code{\\Sexpr\{\}}
  statements is ignored by \code{Stangle}.

  \code{Stangle} is just a
  frontend to \code{Sweave} using a simple driver by default, which
  discards the documentation and concatenates all code chunks the
  current S engine understands.
}   
\author{Friedrich Leisch}
\references{
  Friedrich Leisch: Sweave User Manual, 2002\cr
  \url{http://www.ci.tuwien.ac.at/~leisch/Sweave}
}
\seealso{\code{\link{RweaveLatex}}, \code{\link{Rtangle}}}
\examples{
testfile <- file.path(R.home(), "library", "tools",
                      "Sweave", "test-1.Rnw")

## create a LaTeX file
Sweave(testfile)

## create an S source file from the code chunks
Stangle(testfile)
## which can be simply sourced
source("test-1.R")
}
\keyword{utilities}

