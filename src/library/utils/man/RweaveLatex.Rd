% File src/library/utils/man/RweaveLatex.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

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
                 debug = FALSE, stylepath, \dots)
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
  \item{stylepath}{See \sQuote{Details}.}
  \item{\dots}{named values for the options listed in \sQuote{Supported
      Options}.}
}
\details{
  The LaTeX file generated needs to contain
  \samp{\\usepackage\{Sweave\}}, and if this is not present in the
  Sweave source file, it is inserted by the \code{RweaveLatex} driver.
  If \code{stylepath = TRUE}, a hard-coded path to the file
  \file{Sweave.sty} in the \R installation is set in place of \code{Sweave}.
  The hard-coded path makes the TeX file less portable, but avoids the
  problem of installing the current version of \file{Sweave.sty} to some
  place in your TeX input path.  However, TeX may not be able to
  process the hard-coded path if it contains spaces (as it often will
  under Windows) or TeX special characters.

  The default in \R prior to 2.7.0 was \code{stylepath = TRUE}.  It is
  now taken from the environment variable
  \env{SWEAVE_STYLEPATH_DEFAULT}, or is \code{FALSE} it that is unset or
  empty.  If set, it should be exactly \code{TRUE} or \code{FALSE}: any
  other values are taken as \code{FALSE}.

  By default, \file{Sweave.sty} sets the width of all included graphics to:\cr
  \samp{\\setkeys{Gin}{width=0.8\\textwidth}}.

  This setting affects the width size option passed to the
  \samp{\\includegraphics{}} directive for each plot file and in turn
  impacts the scaling of your plot files as they will appear in your
  final document.
  
  Thus, for example, you may set \code{width=3} in your figure chunk and
  the runtime generated EPS and PDF files will be set to 3 inches in
  width. However, the width of your graphic in your final document, will
  be set to \samp{0.8\\textwidth} and the height dimension will be
  scaled accordingly. Fonts and symbols will be similarly scaled in the
  final document. 
  
  You can adjust the default value by including the
  \samp{\\setkeys{Gin}{width=...}} directive in your .Rnw file after the
  \samp{\\begin{document}} directive and changing the \code{width}
  option value as you prefer, using standard LaTeX measurement values.
 
  If you wish to override this default behavior entirely, you can add a
  \samp{\\usepackage[nogin]{Sweave}} directive in your preamble. In this
  case, no size/scaling options will be passed to the
  \samp{\\includegraphics{}} directive and the \code{height} and
  \code{width} options will determine both the runtime generated graphic
  file sizes and the size of the graphics in your final document.

  \file{Sweave.sty} also supports the \samp{[noae]} option, which
  suppresses the use of the \samp{ae} package, the use of which may
  interfere with certain encoding and typeface selections. If you have
  problems in the rendering of certain character sets, try this option.
}
\section{Supported Options}{
  \code{RweaveLatex} supports the following options for code chunks (the values
  in parentheses show the default values):
  \describe{
    \item{echo:}{logical (\code{TRUE}). Include S code in the
      output file?}
    \item{keep.source:}{logical (\code{FALSE}).  When echoing, if
      \code{keep.source == TRUE} the original source is copied to the file.
      Otherwise, deparsed source is echoed.}
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
    \item{eps:}{logical (\code{TRUE}), indicating whether EPS figures should be
      generated. Ignored if \code{fig = FALSE}.}
    \item{pdf:}{logical (\code{TRUE}), indicating whether PDF figures should be
      generated. Ignored if \code{fig = FALSE}.}
    \item{pdf.version, pdf.encoding:}{passed to \code{\link{pdf}} to set
      the version and encoding.  Defaults taken from \code{pdf.options()}.}
    \item{width:}{numeric (6), width of figures in inches. See \sQuote{Details}.}
    \item{height:}{numeric (6), height of figures in inches. See \sQuote{Details}.}
    \item{expand:}{logical (\code{TRUE}).  Expand references to other chunks
      so that only R code appears in the output file.  If \code{FALSE}, the
      chunk reference (e.g. \code{<<chunkname>>}) will appear.  The
      \code{expand=FALSE} option requires \code{keep.source = TRUE} or it
      will have no effect.}
    \item{concordance:}{logical (\code{FALSE}).  Write a concordance file
      to link the input line numbers to the output line numbers.  This is
      an experimental feature; see the source code for the output format,
      which is subject to change in future releases.}
  }
}
\author{Friedrich Leisch}
\references{
  Friedrich Leisch: Sweave User Manual, 2008\cr
  \url{http://www.stat.uni-muenchen.de/~leisch/Sweave}
}
\seealso{\code{\link{Sweave}}, \code{\link{Rtangle}}}
\keyword{utilities}
