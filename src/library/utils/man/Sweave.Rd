\name{Sweave}
\alias{Sweave}
\alias{Stangle}
\alias{SweaveSyntaxLatex}
\alias{SweaveSyntaxNoweb}
\title{Automatic Generation of Reports}
\description{
  \code{Sweave} provides a flexible framework for mixing text and S code
  for automatic report generation. The basic idea is to replace the S
  code with its output, such that the final document only contains the
  text and the output of the statistical anlysis. 
}
\usage{
Sweave(file, driver = RweaveLatex(),
       syntax = getOption("SweaveSyntax"), ...)

Stangle(file, driver = Rtangle(),
        syntax = getOption("SweaveSyntax"), ...)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{driver}{The actual workhorse, see details below.}
  \item{syntax}{An object of class \code{SweaveSyntax} or a character
    string with its name. The default installation provides
    \code{SweaveSyntaxNoweb} and \code{SweaveSyntaxLatex}.}
  \item{\dots}{Further arguments passed to the driver's setup function.}
}
\details{
  Automatic generation of reports by mixing word processing markup (like
  latex) and S code. The S code gets replaced by its output (text or
  graphs) in the final markup file. This allows to re-generate a report
  if the input data change and documents the code to reproduce the
  analysis in the same file that also produces the report.

  \code{Sweave} combines the documentation and code chunks together
  (or their output) into a single document. \code{Stangle} extracts only
  the code from the Sweave file creating a valid S source file (that can
  be run using \code{\link{source}}). Code inside \code{\\Sexpr\{\}}
  statements is ignored by \code{Stangle}.

  \code{Stangle} is just a
  frontend to \code{Sweave} using a simple driver by default, which
  discards the documentation and concatenates all code chunks the
  current S engine understands.
}   
\section{Hook Functions}{
  Before each code chunk is evaluated, a number of hook functions can be
  executed. If \code{getOption("SweaveHooks")} is set,
  it is taken to be a collection of hook functions. For each logical
  option of a code chunk (\code{echo}, \code{print}, \ldots) a hook can
  be specified, which is executed if and only if the respective option
  is \code{TRUE}. Hooks must be named elements of the list returned by
  \code{getOption("SweaveHooks")} and be functions taking no
  arguments. E.g., if option
  \code{"SweaveHooks"} is defined as \code{list(fig = foo)}, and
  \code{foo} is a function, then
  it would be executed before the code in each
  figure chunk. This is especially useful to set defaults for the
  graphical parameters in a series of figure chunks.

  Note that the user is free to define new Sweave options and associate
  arbitrary hooks with them. E.g., one could define a hook function for
  option \code{clean} that removes all objects in the global
  environment. Then all code chunks with \code{clean=TRUE} would start
  operating on an empty workspace.
}
\section{Syntax Definition}{
  Sweave allows a very flexible syntax framework for marking
  documentation and text chunks. The default is a noweb-style syntax, as
  alternative a latex-style syntax can be used. See the user manual for
  details.
}
\author{Friedrich Leisch}
\references{
  Friedrich Leisch: Sweave User Manual, 2002\cr
  \url{http://www.ci.tuwien.ac.at/~leisch/Sweave}

  Friedrich Leisch: Dynamic generation of statistical reports using
  literate data analysis. In W. Härdle and B. Rönz,
  editors, Compstat 2002 - Proceedings in Computational
  Statistics, pages 575-580. Physika Verlag, Heidelberg, Germany,
  2002. ISBN 3-7908-1517-9.
}
\seealso{\code{\link{RweaveLatex}}, \code{\link{Rtangle}}}
\examples{
testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")

## create a LaTeX file
Sweave(testfile)

## create an S source file from the code chunks
Stangle(testfile)
## which can be simply sourced
source("Sweave-test-1.R")

\dontshow{if(!interactive()) unlink("Sweave-test-1*")}
}
\keyword{utilities}
