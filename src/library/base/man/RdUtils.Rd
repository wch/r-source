% File src/library/base/man/RdUtils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{RdUtils}
\Rdversion{1.1}
\alias{Rdconv}
\alias{Rd2txt}
\alias{Rd2dvi}
\alias{Sd2Rd}
\title{Utilities for Processing Rd Files}
\description{
  Utilities for converting files in R documentation (Rd) format to other
  formats or create indices from them, and for converting documentation
  in other formats to Rd format.
}
\usage{

\special{R CMD Rdconv [options] file}
\special{R CMD Rd2dvi [options] files}
\special{R CMD Rd2txt [options] file}
\special{R CMD Sd2Rd [options] file}
}
\arguments{
  \item{file}{the path to a file to be processed.}
  \item{files}{a list of file names specifying the R documentation
    sources to use, by either giving the paths to the files, or the path
    to a directory with the sources of a package.}
  \item{options}{further options to control the processing, or for
    obtaining information about usage and version of the utility.}
}
\details{
  \code{Rdconv} converts Rd format to other formats.  Currently,
  plain text, HTML and LaTeX formats are supported.  It can also extract
  the examples for run-time testing.

  \code{Rd2dvi} and \code{Rd2txt} are user-level programs
  for producing DVI/PDF output or pretty text output from Rd sources.
  \code{Rd2dvi} will make use of environment variables
#ifdef unix
  \env{R_PAPERSIZE} (set by \code{R CMD}, with a default set when \R was
  installed) and \env{xdvi} (the DVI previewer, default \code{xdvi}),
#endif
#ifdef windows
  \env{R_PAPERSIZE} (default \code{a4}, ) and \env{xdvi} (the DVI
  previewer, default \code{open} (use file associations)),
#endif
  and \env{R_PDFVIEWER} (the PDF previewer).  (Valid values for
  \env{R_PAPERSIZE} are \code{a4}, \code{letter}, \code{legal} and
  \code{executive}.)

  \code{Sd2Rd} converts S (version 3 or 4) documentation formats to Rd
  format: it is no longer much used and the results may need hand-tuning.

  Use \code{R CMD foo --help} to obtain usage information on utility \code{foo}.
}
\seealso{
  The chapter \dQuote{Processing Rd format} in the \dQuote{Writing \R
    Extensions} manual.
}
\keyword{utilities}
