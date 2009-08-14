% File src/library/tools/man/texi2dvi.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{texi2dvi}
\alias{texi2dvi}
\title{Compile LaTeX Files}
\description{
  Run \code{latex} and \code{bibtex} until all cross-references are
  resolved and create either a dvi or PDF file.
}
\usage{
texi2dvi(file, pdf = FALSE, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"), texinputs = NULL)
}
\arguments{
  \item{file}{character.  Name of LaTeX source file.}
  \item{pdf}{logical.  If \code{TRUE}, a PDF file is produced instead of
    the default dvi file (\code{texi2dvi} command line option \option{--pdf}).}
  \item{clean}{logical.  If \code{TRUE}, all auxiliary files are removed
    (\code{texi2dvi} command line option \option{--clean}).  May not work on
    some platforms.}
  \item{quiet}{logical.  No output unless an error occurs.  Ignored if
    emulation (see the \code{texi2dvi} argument) is used.}
  \item{texi2dvi}{character (or \code{NULL}).  Script or program used to
    compile a TeX file to dvi or PDF, respectively.
    The default (selected by \code{""} or \code{NULL}) is to look for an
    executable on the search path and otherwise emulate the script with
    \code{system} calls.}
  \item{texinputs}{\code{NULL} or a character vector of paths to add to
    the LaTeX and bibtex input search paths.}
}
\details{
  Despite the name, this is used in \R to compile LaTeX files,
  specifically those generated from vignettes.  It
  ensures that the \file{\var{\link{R_HOME}}/share/texmf} directory is
  in the \env{TEXINPUTS} path, so \R style files such as \file{Sweave}
  and \file{Rd} will be found.  The search path used is first the
  existing \env{TEXINPUTS} setting (or the current directory if unset),
  then elements of \code{texinputs}, then
  \file{\var{R_HOME}/share/texmf} and finally the default
  path. Analogous changes are made to \env{BIBINPUTS} and
  \env{BSTINPUTS} settings.
#ifdef windows
  
  MiKTeX has a \code{texi2dvi} executable but no other Windows TeX
  installation that we know of does, so emulation is used on e.g. TeXLive
  installations.
#endif
}
\author{Achim Zeileis and R-core}
\keyword{utilities}
