\name{RweaveLatex}
\alias{RweaveLatex}
\alias{RweaveLatexSetup}
\title{R/LaTeX Driver for Sweave}
\description{
  A driver for \code{\link{Sweave}} that translates R code chunks in
  LaTeX files.
}
\usage{
RweaveLatex()
RweaveLatexSetup(file, output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             split=FALSE, stylepath=TRUE, pdf=TRUE, eps=TRUE
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{output}{Name of output file, default is to remove extension
   \file{.nw}, \file{.Rnw} or \file{.Snw} and to add extension \file{.tex}. Any
   directory names in \code{file} are also remogved such that the output
   is created in the current working directory.}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
  \item{debug}{If \code{TRUE}, input and output of all code
   chunks is copied to the console.}
  \item{echo}{By default S commands are echoed to the output file, if
    \code{FALSE}, only their output is included.}
  \item{split}{If \code{TRUE}, text output is written to seperate files
    for each code chunk.}
  \item{stylepath}{If \code{TRUE}, a hard path to the file \file{Sweave.sty}
    installed with this package is set, if \code{FALSE}, only
    \code{\\usepackage\{Sweave\}} is written. The hard path makes the
    TeX file less
    portable, but avoids the problem of installing the current
    version of \file{Sweave.sty} to some place in your TeX input path.}
  \item{pdf}{Create PDF graphics (for use by pdflatex)?}
  \item{eps}{Create EPS graphics (for use by normal latex)?}
}
\author{Friedrich Leisch}
\references{
  Friedrich Leisch: Sweave User Manual, 2002\cr
  \url{http://www.ci.tuwien.ac.at/~leisch/Sweave}
}
\seealso{\code{\link{Sweave}}, \code{\link{Rtangle}}}
\keyword{utilities}
