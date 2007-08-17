% File src/library/tools/man/texi2dvi.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{texi2dvi}
\alias{texi2dvi}
\title{Compile LaTeX Files}
\description{
  Run latex and bibtex until all cross-references are
  resolved and create either a dvi or PDF file.
}
\usage{
texi2dvi(file, pdf = FALSE, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"))
}
\arguments{
  \item{file}{character.  Name of LaTeX source file.}
  \item{pdf}{logical.  If \code{TRUE}, a PDF file is produced instead of
    the default dvi file (\code{texi2dvi} command line option \option{--pdf}).}
  \item{clean}{logical.  If \code{TRUE}, all auxiliary files are removed
    (\code{texi2dvi} command line option \option{--clean}).  May not work on
    some platforms.}
  \item{quiet}{logical.  No output unless an error occurs.}
  \item{texi2dvi}{character (or \code{NULL}).  Script or program used to
    compile a TeX file to dvi or PDF, respectively.
    The default (selected by \code{NULL}) is to look for an executable
    on the search path and otherwise emulate the script with
    \code{system} calls.}
}
\details{
  Despite the name, this is used in \R to compile LaTeX files,
  specifically those generated from vignettes.
#ifdef windows
  
  Most Windows TeX installations do not have a \code{texi2dvi} executable,
  but MiKTeX does.
#endif
}
\author{Achim Zeileis}
\keyword{utilities}
