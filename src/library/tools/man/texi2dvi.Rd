% File src/library/tools/man/texi2dvi.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{texi2dvi}
\alias{texi2dvi}
\alias{texi2pdf}
\title{Compile LaTeX Files}
\description{
  Run \code{latex}/\code{pdflatex}, \code{makeindex} and \code{bibtex}
  until all cross-references are resolved to create a \abbr{DVI} or a PDF file.
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
  \item{file}{character string.  Name of the LaTeX source file.}
  \item{pdf}{logical.  If \code{TRUE}, a PDF file is produced instead of
    the default \abbr{DVI} file (\code{texi2dvi} command line option \option{--pdf}).}
  \item{clean}{logical.  If \code{TRUE}, all auxiliary files created
    during the conversion are removed.}
  \item{quiet}{logical.  No output unless an error occurs
    (\code{texi2dvi} command-line option \option{--quiet}).}
  \item{texi2dvi}{character string (or \code{NULL}).  Script or program
    used to compile a TeX file to \abbr{DVI} or PDF. The default (selected
    by \code{""} or \code{"texi2dvi"} or \code{NULL}) is to look
    for a program or script named \command{texi2dvi} on the path and
    otherwise emulate the script with \code{system2} calls (which
    can be selected by the value \code{"emulation"}).  See also
    \sQuote{Details}.} 
  \item{texinputs}{\code{NULL} or a character vector of paths to prepend to
    the LaTeX and BibTeX input search paths.}
  \item{index}{logical: should indices be prepared?}
}
\details{
  \code{texi2pdf} is a wrapper for the common case of
  \code{texi2dvi(pdf = TRUE)}.

  Despite the name, this is used in \R to compile \LaTeX files,
  specifically those generated from vignettes and by the
  \code{\link{Rd2pdf}} script (used for package reference manuals).  It
  ensures that the \file{\var{\link{R_HOME}}/share/texmf} directory is
  in the \env{TEXINPUTS} path, so \R style files such as \file{Sweave.sty}
  and \file{Rd.sty} will be found.  The TeX search path used is first the
  elements of argument \code{texinputs}, then the
  existing \env{TEXINPUTS} setting (or the current directory if unset),
  then \file{\var{R_HOME}/share/texmf} and finally the system paths.
  Analogous changes are made to \env{BIBINPUTS} and \env{BSTINPUTS}
  settings.

  On Unix-alikes,
  the default option for \code{texi2dvi} is set from environment variable
  \env{R_TEXI2DVICMD}, and the default for that is set from environment
  variable \env{TEXI2DVI} or if that is unset, from a value chosen when
  \R is configured.
  
  On Windows, the option \code{"texi2dvi"} is normally unset.  The \I{MiKTeX}
  command \command{texify.exe} is a suitable choice for \code{texi2dvi},
  and is tried as from \R 3.2.3 if the argument is blank or \code{NULL}
  and a program or script \command{texi2dvi} is not found.
  %% \I{MiKTeX} had a \code{texi2dvi} executable until Sept 2015, but no other
  %% Windows TeX installation that we know of does, so emulation is used on
  %% e.g.\sspace{}\I{TeXLive} installations on Windows.
  %% Update: texi2dvi is now again available and used in the Msys2 toolchain.

  A shell script \command{texi2dvi} is part of GNU \I{Texinfo}.
  If another external script is used as the \code{texi2dvi} command
  it should accept options \option{--pdf} (for \code{pdf = TRUE}) and
  \option{--quiet} (for \code{quiet = TRUE}), but also \option{--help}
  and \option{--version}.  This is true for the \command{latexmk} Perl
  script available from \LaTeX package managers.
  %% but seems to be slower than the other approaches.

  Occasionally indices contain special characters which cause indexing
  to fail (particularly when using the \samp{hyperref} LaTeX package)
  even on valid input.  The argument \code{index = FALSE} is provided to
  allow package manuals to be made when this happens: it uses emulation.
}

\note{
  %% configure just looks for a texi2dvi command and does not check its
  %% version.  However, it is normally distributed with texinfo, and
  %% that is checked to be >= 6.1.
  %% The macOS distribution by default installs /usr/local/bin/texi2dvi,
  %% but that is optional and macOS has /usr/bin/texi2dvi stuck at 4.8.
  There are various versions of the \command{texi2dvi} script on
  Unix-alikes and quite a number of bugs have been seen, some of which
  this \R wrapper works around, but if yours does not work correctly
  try \code{texi2dvi = "emulation"}.
  %% issues included not re-running latex in texinfo 6.1 and bashisms in
  %% the /bin/sh script in <= 6.0.

  The historic \command{texi2dvi} version \code{4.8} (as
  supplied by macOS)% FIXME: is this version still shipped?
  will not work correctly for paths which
  contain spaces, nor if the absolute path to a file would contain
  spaces.
  It removes ancillary files that already exist, whereas later versions
  and other compilation approaches do not (and may get confused by
  such files).

  The command-line option \option{--max-iterations=20} is no longer used:
  all supported scripts nowadays avoid infinite retries by default.
  %% texi2dvi >= 6.0 stops after 7, texify and latexmk after 5
  %% iterations (but the latter uses a configuration variable).

  In emulation mode, \code{clean = TRUE} only cleans up if the
  conversion was successful---this gives users a chance to examine log
  files in the event of error.
  
  All the approaches should respect the values of environment variables
  \env{LATEX}, \env{PDFLATEX}, \env{MAKEINDEX} and \env{BIBTEX} for the
  full paths to the corresponding commands.
}
\value{
  Invisible \code{NULL}.  Used for the side effect of creating a \abbr{DVI} or
  PDF file in the current working directory (and maybe other files,
  especially if \code{clean = FALSE}).
}
\author{
  Originally \I{Achim Zeileis} but largely rewritten by R-core.
}
\keyword{utilities}
