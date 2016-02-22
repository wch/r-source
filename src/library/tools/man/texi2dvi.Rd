% File src/library/tools/man/texi2dvi.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{texi2dvi}
\alias{texi2dvi}
\alias{texi2pdf}
\title{Compile LaTeX Files}
\description{
  Run \code{latex} and \code{bibtex} until all cross-references are
  resolved and create either a dvi or a PDF file.
}
\usage{
texi2dvi(file, pdf = FALSE, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"),
         texinputs = NULL, index = TRUE)

texi2pdf(file, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"),
         texinputs = NULL, index = TRUE)
}
\arguments{
  \item{file}{character string.  Name of LaTeX source file.}
  \item{pdf}{logical.  If \code{TRUE}, a PDF file is produced instead of
    the default dvi file (\code{texi2dvi} command line option \option{--pdf}).}
  \item{clean}{logical.  If \code{TRUE}, all auxiliary files created
    during the conversion are removed.}
  \item{quiet}{logical.  No output unless an error occurs.}
  \item{texi2dvi}{character string (or \code{NULL}).  Script or program used to
    compile a TeX file to dvi or PDF.
    The default (selected by \code{""} or \code{"texi2dvi"} or
    \code{NULL}) is to look for a program or script named
    \file{texi2dvi} on the path and otherwise emulate the script with
    \code{system2} calls (which can be selected by the value
    \code{"emulation"}).  See also \sQuote{Details}.} 
  \item{texinputs}{\code{NULL} or a character vector of paths to add to
    the LaTeX and bibtex input search paths.}
  \item{index}{logical: should indices be prepared?}
}
\details{
  \code{texi2pdf} is a wrapper for the common case of
  \code{texi2dvi(pdf = TRUE)}.

  Despite the name, this is used in \R to compile LaTeX files,
  specifically those generated from vignettes.  It
  ensures that the \file{\var{\link{R_HOME}}/share/texmf} directory is
  in the \env{TEXINPUTS} path, so \R style files such as \file{Sweave}
  and \file{Rd} will be found.  The TeX search path used is first the
  existing \env{TEXINPUTS} setting (or the current directory if unset),
  then elements of \code{texinputs}, then
  \file{\var{R_HOME}/share/texmf} and finally the default
  path.  Analogous changes are made to \env{BIBINPUTS} and
  \env{BSTINPUTS} settings.

#ifdef unix
  The default option for \code{texi2dvi} is set from environment variable
  \env{R_TEXI2DVICMD}, and the default for that is set from environment
  variable \env{TEXI2DVI} or if that is unset, from a value chosen when
  \R is configured.
  
  %% issues included not re-running latex in texinfo 6.1 and bashisms in
  %% the /bin/sh script in <= 6.0.
  A shell script \command{texi2dvi} is part of GNU's \pkg{texinfo}.
  Several issues have been seen with released versions, so if yours does
  not work correctly set \env{R_TEXI2DVICMD=emulation}.
#endif

#ifdef windows
  On Windows the option \code{"texi2dvi"} is normally unset.  The MiKTeX
  command \command{texify.exe} is a suitable choice for \code{texi2dvi},
  and is tried as from \R 3.2.3 if the argument is blank or \code{NULL}
  and \code{texi2dvi} is not found.

  MiKTeX had a \code{texi2dvi} executable until Sept 2015, but no other
  Windows TeX installation that we know of does, so emulation is used on
  e.g.\sspace{}TeXLive installations on Windows.
#endif

  Occasionally indices contain special characters which cause indexing
  to fail (particularly when using the \samp{hyperref} LaTeX package)
  even on valid input.  The argument \code{index = FALSE} is provided to
  allow package manuals to be made when this happens: it uses emulation.
}

\note{
  There are various versions of the \command{texi2dvi} script on
  Unix-alikes and quite a number of bugs have been seen, some of which
  this \R wrapper works around.

  One that was present with \command{texi2dvi} version \code{4.8} (as
  supplied by OS X) is that it will not work correctly for paths which
  contain spaces, nor if the absolute path to a file would contain
  spaces.

  The three possible approaches all have their quirks.  For example the
  Unix-alike \code{texi2dvi} script removes ancillary files that already
  exist but the other two approaches do not (and may get confused by
  such files).
  
  Where supported (\command{texi2dvi} 5.0 and later, and
  \command{texi2dvi.exe} and \command{texify.exe} from MiKTeX), option
  \option{--max-iterations=20} is used to avoid infinite retries.
  
  The emulation mode supports \code{quiet = TRUE} from \R 3.2.3 only.
}
\value{
  Invisible \code{NULL}.  Used for the side effect of creating a dvi or
  PDF file in the current working directory (and maybe other files,
  especially if \code{clean = FALSE}).
}
\author{
  Originally Achim Zeileis but largely rewritten by R-core.
}
\keyword{utilities}
