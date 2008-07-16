% File src/library/utils/man/RweaveLatex.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{RweaveRd}
\alias{RweaveRd}
\alias{RweaveRdSetup}
\title{Rd Driver for Sweave}
\description{
  A driver for \code{\link{Sweave}} that translates R code chunks in
  Rd man pages.
}
\usage{
RweaveRd()

RweaveRdSetup(file, syntax=RweaveRdSyntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, keep.source=TRUE, split=FALSE,
             pdf=FALSE, eps=FALSE)
}
\arguments{
  \item{file}{Name of Sweave source file.}
  \item{syntax}{An object of class \code{SweaveSyntax}.}
  \item{output}{Name of output file, default is to remove extension
    \file{.Rdw} and to add extension \file{.Rd}. Any 
    directory names in \code{file} are also removed such that the output
    is created in the current working directory.}
  \item{quiet}{If \code{TRUE} all progress messages are suppressed.}
  \item{debug}{If \code{TRUE}, input and output of all code
    chunks is copied to the console.}
  \item{stylepath}{See \sQuote{Details}.}
  \item{echo}{set default for option \code{echo}, see details below.}
  \item{eval}{set default for option \code{eval}, see details below.}
  \item{keep.source}{set default for option \code{keep.source}, see
    details below.}
  \item{split}{set default for option \code{split}, see details below.}
  \item{pdf}{set default for option \code{pdf}, see details below.}
  \item{eps}{set default for option \code{eps}, see details below.}
}
\details{

  Man pages in R packages may be written in either \code{.Rd} format
or \code{.Rdi} format. If the latter, then at the time the package is
installed, \code{Sweave} will be used with this driver to convert the
page to \code{.Rd} format.

  The \code{RweaveRdSyntax} currently defaults to a style of syntax
similar to \code{\link{RweaveLatex}}.  Code blocks are wrapped in
\code{\\begin{atInstall}} and \code{\\end{atInstall}}, with inline
expressions marked using \code{\\atInstall}.  In the future it is
planned to use other markup to indicate code that should run at
package build time before installation, package load time after
installation, and/or man page display time.
  
  Currently neither PDF nor EPS inclusions are supported by the target
Rd format, but the \code{pdf} and \code{eps} options (and some others;
see below) are left in place for future expansion.

}
\section{Supported (and Incompletely Supported) Options}{
  \code{RweaveRd} supports the following options for code chunks (the values
  in parentheses show the default values):
  \describe{
    \item{echo:}{logical (\code{TRUE}). Include S code in the
      output file?}
    \item{keep.source:}{logical (\code{FALSE}).  When echoing, if \code{keep.source == TRUE}
      the original source is copied to the file.  Otherwise, deparsed source is echoed.}
    \item{eval:}{logical (\code{TRUE}). If \code{FALSE}, the code chunk is not
      evaluated, and hence no text or graphical output produced.}
    \item{results:}{character string (\code{verbatim}).
      If \code{verbatim}, the output of S commands is
      included in the verbatim-like \code{\\preformatted} environment.  
      If \code{hide} then all output is
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
      for each code chunk.  This is not currently supported by the target Rd format.}
    \item{strip.white:}{character string (\code{false}). If \code{true}, blank
      lines at the beginning and end of output are removed. If
      \code{all}, then all blank lines are removed from the output.}
    \item{prefix:}{logical (\code{TRUE}). If \code{TRUE} generated filenames of
      figures and output have a common prefix.}
    \item{prefix.string:}{a character string, default is the name of the
      \file{.Rdw} source file.}
    \item{include:}{logical (\code{TRUE}), indicating whether input
      statements for text output and includegraphics statements
      for figures should be auto-generated. Use \code{include = FALSE} if
      the output should appear in a different place than the code chunk
      (by placing the input line manually).  This is not currently supported
      by the Rd format.}
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
    \item{expand:}{logical (\code{TRUE}).  Expand references to other chunks
      so that only R code appears in the output file.  If \code{FALSE}, the
      chunk reference (e.g. \code{<<chunkname>>}) will appear.  The 
      \code{expand=FALSE} option requires \code{keep.source = TRUE} or it 
      will have no effect.}
    \item{concordance:}{logical (\code{FALSE}).  Write a concordance file
      to link the input line numbers to the output line numbers.  This is
      an experimental feature; see the source code for the output format,
      which is subject to change in future releases, and is not currently supported
      in the Rd format.}
  }
}
\author{Friedrich Leisch and Duncan Murdoch}
\references{
  Friedrich Leisch: Sweave User Manual, 2008\cr
  \url{http://www.stat.uni-muenchen.de/~leisch/Sweave}
}
\seealso{\code{\link{Sweave}}, \code{\link{Rtangle}}}
\keyword{utilities}
