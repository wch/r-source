% File src/library/utils/man/Sweave.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{Sweave}
\alias{Sweave}
\alias{Stangle}
\alias{SweaveSyntaxLatex}
\alias{SweaveSyntaxNoweb}
\encoding{UTF-8}
\title{Automatic Generation of Reports}
\description{
  \code{Sweave} provides a flexible framework for mixing text and R/S code
  for automatic report generation.  The basic idea is to replace the
  code with its output, such that the final document only contains the
  text and the output of the statistical analysis: however, the source
  code can also be included.
}
\usage{
Sweave(file, driver = RweaveLatex(),
       syntax = getOption("SweaveSyntax"), encoding = "", ...)

Stangle(file, driver = Rtangle(),
        syntax = getOption("SweaveSyntax"), encoding = "", ...)
}
\arguments{
  \item{file}{Path to Sweave source file.  Note that this can be
    supplied without the extension, but the function will only proceed
    if there is exactly one Sweave file in the directory whose
    basename matches \code{file}.}
  \item{driver}{the actual workhorse, (a function returning) a named
    \code{\link{list}} of five functions; for details, see Section 5 of the
    \sQuote{Sweave User Manual} available as \code{vignette("Sweave")}.}
  \item{syntax}{\code{NULL} or an object of class \code{"SweaveSyntax"} or
    a character string with its name.
    See the section \sQuote{Syntax Definition}.}
  \item{encoding}{The default encoding to assume for \code{file}.}
  \item{\dots}{further arguments passed to the driver's setup function.
    See \code{\link{RweaveLatexSetup}} and \code{\link{RtangleSetup}},
    respectively, for the arguments of the default drivers.}
}

\details{
  An Sweave source file contains both text in a markup language (like
  \LaTeX) and \R (or S) code.  The code gets replaced by its output (text or
  graphs) in the final markup file.  This allows a report to be re-generated
  if the input data change and documents the code to reproduce the
  analysis in the same file that also produces the report.

  \code{Sweave} combines the documentation and code chunks (or
  their output) into a single document.  \code{Stangle} extracts only
  the code from the Sweave file creating an \R source file that can be
  run using \code{\link{source}}.  (Code inside \code{\\Sexpr\{\}}
  statements is ignored by \code{Stangle}.)
  
  \code{Stangle} is just a wrapper to \code{Sweave} specifying a
  different default driver.  Alternative drivers can be used and are
  provided by various contributed packages.

  Environment variable \env{SWEAVE_OPTIONS} can be used to override the
  initial options set by the driver: it should be a comma-separated set
  of \code{key=value} items, as would be used in a \samp{\\SweaveOpts}
  statement in a document.

  If the \code{encoding} is unspecified (the default),
  \emph{non-ASCII} source files must contain a line of the form
\preformatted{  \usepackage[foo]{inputenc}}
  (where \samp{foo} is typically \samp{latin1}, \samp{latin2}, \samp{utf8} or
  \samp{cp1252} or \samp{cp1250}) or a comment line
  \preformatted{  \%\SweaveUTF8}
  to declare UTF-8 input (the default encoding assumed by \I{pdfTeX} since 2018),
  or they will give an error.
  Re-encoding can be turned off completely with argument
  \code{encoding = "bytes"}.
}

\section{Syntax Definition}{
  Sweave allows a flexible syntax framework for marking
  documentation and text chunks. The default is a \I{noweb}-style syntax, as
  alternative a \LaTeX-style syntax can be used.  (See the user manual for
  further details.)

  If \code{syntax = NULL} (the default) then the available syntax
  objects are consulted in turn, and selected if their \code{extension}
  component matches (as a regexp) the file name.  Objects
  \code{SweaveSyntaxNoweb} (with \code{extension = "[.][rsRS]nw$"}) and
  \code{SweaveSyntaxLatex} (with \code{extension = "[.][rsRS]tex$"}) are
  supplied, but users or packages can supply others with names matching
  the pattern \code{SweaveSyntax.*}.
}

\author{
  Friedrich Leisch and R-core.
}

\references{
  Friedrich Leisch (2002)
  Dynamic generation of statistical reports using literate data analysis.
  In W. \enc{Härdle}{Haerdle} and B. \enc{Rönz}{Roenz}, editors,
  \emph{Compstat 2002 - Proceedings in Computational Statistics},
  pages 575--580. Physika Verlag, Heidelberg, Germany, ISBN 3-7908-1517-9.
}

\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.

  \code{\link{RweaveLatex}}, \code{\link{Rtangle}}.
  Alternative Sweave drivers are in, for example, packages
  \pkg{weaver} (Bioconductor), \CRANpkg{R2HTML}, and \CRANpkg{ascii}.

  \code{tools::\link[tools]{buildVignette}} to process source files
  using Sweave or alternative vignette processing engines.
}

\examples{
testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")
\dontshow{oldwd <- setwd(tempdir()) # so we will write only to a temp directory}

## enforce par(ask = FALSE)
options(device.ask.default = FALSE)

## create a LaTeX file - in the current working directory, getwd():
Sweave(testfile)

## This can be compiled to PDF by
## tools::texi2pdf("Sweave-test-1.tex")

## or outside R by
##
## 	R CMD texi2pdf Sweave-test-1.tex
## on Unix-alikes which sets the appropriate TEXINPUTS path.
##
## On Windows,
##      Rcmd texify --pdf Sweave-test-1.tex
## if MiKTeX is available.

## create an R source file from the code chunks
Stangle(testfile)
## which can be sourced, e.g.
source("Sweave-test-1.R")

\dontshow{
if(!interactive()) unlink("Sweave-test-1*")
setwd(oldwd)
}
}
\keyword{utilities}
