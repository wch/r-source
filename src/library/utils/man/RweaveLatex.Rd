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

RweaveLatexSetup(file, syntax, output = NULL, quiet = FALSE,
                 debug = FALSE, echo = TRUE, eval  =  TRUE,
                 split = FALSE, stylepath = TRUE,
                 pdf = TRUE, eps = TRUE)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{syntax}{An object of class \code{SweaveSyntax}.}
  \item{output}{Name of output file, default is to remove extension
    \file{.nw}, \file{.Rnw} or \file{.Snw} and to add extension
    \file{.tex}. Any 
    directory names in \code{file} are also removed such that the output
    is created in the current working directory.}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
  \item{debug}{If \code{TRUE}, input and output of all code
    chunks is copied to the console.}
  \item{stylepath}{If \code{TRUE}, a hard path to the file \file{Sweave.sty}
    installed with this package is set, if \code{FALSE}, only
    \code{\\usepackage\{Sweave\}} is written. The hard path makes the
    TeX file less
    portable, but avoids the problem of installing the current
    version of \file{Sweave.sty} to some place in your TeX input path.
    The argument is ignored if a \code{\\usepackage\{Sweave\}} is
    already present in the Sweave source file.
  }
  \item{echo}{set default for option \code{echo}, see details below.}
  \item{eval}{set default for option \code{eval}, see details below.}
  \item{split}{set default for option \code{split}, see details below.}
  \item{pdf}{set default for option \code{pdf}, see details below.}
  \item{eps}{set default for option \code{eps}, see details below.}
}
\section{Supported Options}{
  RweaveLatex supports the following options for code chunks (the values
  in parentheses show the default values):
  \describe{
    \item{echo:}{logical (\code{TRUE}). Include S code in the
      output file?}
    \item{eval:}{logical (\code{TRUE}). If \code{FALSE}, the code chunk is not
      evaluated, and hence no text or graphical output produced.}
    \item{results:}{character string (\code{verbatim}).
      If \code{verbatim}, the output of S commands is
      included in the verbatim-like Soutput environment. If
      \code{tex}, the output is taken to be already proper latex markup
      and included as is. If \code{hide} then all output is
      completely suppressed (but the code executed during the weave).}
    \item{print:}{logical (\code{FALSE})
      If \code{TRUE}, each expression in the
      code chunk is wrapped into a \code{print()} statement before evaluation,
      such that the values of all expressions become visible.}
    \item{term:}{logical (\code{TRUE}). If \code{TRUE}, visibility of values
      emulates an interactive R session: values of assignments are not
      printed, values of single objects are printed. If \code{FALSE},
      output comes only from explicit \code{\link{print}} or
      \code{\link{cat}} statements.}
    \item{split:}{logical (\code{FALSE}). If \code{TRUE}, text output is
      written to separate files 
      for each code chunk.}
    \item{strip.white:}{character string (\code{false}). If \code{true}, blank
      lines at the beginning and end of output are removed. If
      \code{all}, then all blank lines are removed from the output.}
    \item{prefix:}{logical (\code{TRUE}). If \code{TRUE} generated filenames of
      figures and output have a common prefix.}
    \item{prefix.string:}{a character string, default is the name of the
      \file{.Snw} source file.}
    \item{include:}{logical (\code{TRUE}), indicating whether input
      statements for text output and includegraphics statements
      for figures should be auto-generated. Use \code{include = FALSE} if
      the output should appear in a different place than the code chunk
      (by placing the input line manually).}
    \item{fig:}{logical (\code{FALSE}), indicating whether the code
      chunk produces 
      graphical output. Note that only one figure per code chunk can be
      processed this way.}
    \item{eps:}{logical (\code{TRUE}), indicating whether EPS figures shall be
      generated. Ignored if \code{fig = FALSE}.} 
    \item{pdf:}{logical (\code{TRUE}), indicating whether PDF figures shall be
      generated. Ignored if \code{fig = FALSE}.}
    \item{width:}{numeric (6), width of figures in inch.}
    \item{height:}{numeric (6), height of figures in inch.}
  }
}
\author{Friedrich Leisch}
\references{
  Friedrich Leisch: Sweave User Manual, 2002\cr
  \url{http://www.ci.tuwien.ac.at/~leisch/Sweave}
}
\seealso{\code{\link{Sweave}}, \code{\link{Rtangle}}}
\keyword{utilities}
