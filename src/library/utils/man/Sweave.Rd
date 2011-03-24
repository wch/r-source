% File src/library/utils/man/Sweave.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

\name{Sweave}
\alias{Sweave}
\alias{Stangle}
\alias{SweaveSyntaxLatex}
\alias{SweaveSyntaxNoweb}
\encoding{UTF-8}
\title{Automatic Generation of Reports}
\description{
  \code{Sweave} provides a flexible framework for mixing text and S code
  for automatic report generation.  The basic idea is to replace the S
  code with its output, such that the final document only contains the
  text and the output of the statistical analysis. 
}
\usage{
Sweave(file, driver = RweaveLatex(),
       syntax = getOption("SweaveSyntax"), ...)

Stangle(file, driver = Rtangle(),
        syntax = getOption("SweaveSyntax"), ...)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{driver}{The actual workhorse, see \sQuote{Details}.}
  \item{syntax}{\code{NULL} or an object of class \code{SweaveSyntax} or
    a character string with its name.  See section \sQuote{Syntax Definition}.}
  \item{\dots}{Further arguments passed to the driver's setup function:
    see section \sQuote{Drivers}, \code{\link{RweaveLatex}} and
    \code{\link{Rtangle}}.}
}

\details{
  Automatic generation of reports by mixing word processing markup (like
  latex) and S code.  The S code gets replaced by its output (text or
  graphs) in the final markup file.  This allows a report to be re-generated
  if the input data change and documents the code to reproduce the
  analysis in the same file that also produces the report.

  \code{Sweave} combines the documentation and code chunks together (or
  their output) into a single document.  \code{Stangle} extracts only
  the code from the Sweave file creating an S source file that can be
  run using \code{\link{source}}.  (Code inside \code{\\Sexpr\{\}}
  statements is ignored by \code{Stangle}.)

  \code{Stangle} is just a wrapper to \code{Sweave} specifying a
  different default driver.  Alternative drivers can be used: the CRAN
  package \pkg{cacheSweave} and the Bioconductor package \pkg{weaver}
  both provide drivers based on the default driver
  \code{\link{RweaveLatex}} which incorporate ideas of \emph{caching}
  the results of computations on code chunks.
}

\section{Hook Functions}{
  Hook functions are part of the driver, so this section applies to the
  default drivers for \code{Sweave} and \code{Stangle} but maybe not for
  others.

  Before each code chunk is evaluated, zero or more hook functions can
  be executed.  If \code{getOption("SweaveHooks")} is set, it is taken
  to be a named list of hook functions.  For each logical option of a
  code chunk (\code{echo}, \code{print}, \ldots) a hook can be
  specified, which is executed if and only if the respective option is
  \code{TRUE}.  Hooks must be named elements of the list returned by
  \code{getOption("SweaveHooks")} and be functions taking no arguments.
  E.g., if option \code{"SweaveHooks"} is defined as \code{list(fig =
  foo)}, and \code{foo} is a function, then it would be executed before
  the code in each figure chunk.  This is especially useful to set
  defaults for the graphical parameters in a series of figure chunks.

  Note that the user is free to define new Sweave logical options and
  associate arbitrary hooks with them.  E.g., one could define a hook
  function for a new option called \code{clean} that removes all objects
  in the workspace.  Then all code chunks specified with \code{clean =
    TRUE} would start operating on an empty workspace.
}

\section{Syntax Definition}{
  Sweave allows a flexible syntax framework for marking
  documentation and text chunks. The default is a noweb-style syntax, as
  alternative a latex-style syntax can be used.  (See the user manual for
  further details.)

  If \code{syntax = NULL} (the default) then the available syntax
  objects are consulted in turn, and selected if there \code{extension}
  component is matches (as a regexp) the file name.  Objects
  \code{SweaveSyntaxNoweb} (with \code{extension = "\\\\.[rsRS]nw$"}) and
  \code{SweaveSyntaxLatex} (with \code{extension = "\\\\.[rsRS]tex$"}) are
  supplied, but users or packages can supply others with names matching
  the pattern \code{SweaveSyntax.*}.
}

%% This should really be in the Sweave manual, if we can ever get hold
%% of it!
\section{Drivers}{
  An Sweave driver is a function which returns a list of functions:
  \describe{
    \item{\code{setup(file, syntax, \dots)}:}{Set up the driver, e.g. open
      the output file.}
    \item{\code{runcode(object, chunk, options)}:}{Process a code chunk.}
    \item{\code{writedoc(object, chunk)}:}{Write out a documentation chunk.}
    \item{\code{finish(object, error)}:}{Finish up, or clean up if
      \code{error} is true.}
    \item{\code{checkopts(options)}:}{Converts/validates \code{options}
      given as a named list of character strings.}
  }
}

\author{
  Friedrich Leisch and R-core.
}

\references{
  Friedrich Leisch: Dynamic generation of statistical reports using
  literate data analysis. In W. \enc{Härdle}{Haerdle} and
  B. \enc{Rönz}{Roenz}, editors, Compstat 2002 - Proceedings in Computational
  Statistics, pages 575--580. Physika Verlag, Heidelberg, Germany,
  2002. ISBN 3-7908-1517-9.
  
  Friedrich Leisch: Sweave User Manual, 2008\cr
  \url{http://www.stat.uni-muenchen.de/~leisch/Sweave}
}

\seealso{
  \code{\link{RweaveLatex}}, \code{\link{Rtangle}}
  
  Packages \pkg{cacheSweave}, \pkg{weaver} and \pkg{SweaveListingUtils}.
}

\examples{
testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package = "utils")

## enforce par(ask=FALSE)
options(device.ask.default=FALSE)

## create a LaTeX file
Sweave(testfile)

## This can be compiled to PDF by
## tools::texi2dvi("Sweave-test-1.tex", pdf=TRUE)
## or outside R by
#ifdef unix
## R CMD texi2dvi --pdf Sweave-test-1.tex
## which sets the appropriate TEXINPUTS path.
#endif
#ifdef windows
## Rcmd texify --pdf Sweave-test-1.tex
## if MiKTeX is available.
#endif

## create an S source file from the code chunks
Stangle(testfile)
## which can be sourced, e.g.
source("Sweave-test-1.R")

\dontshow{if(!interactive()) unlink("Sweave-test-1*")}
}
\keyword{utilities}
