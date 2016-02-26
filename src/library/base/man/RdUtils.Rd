% File src/library/base/man/RdUtils.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{RdUtils}
\alias{Rdconv}
\alias{Rd2pdf}
\alias{RD2PDF_INPUTENC}

\title{Utilities for Processing Rd Files}
\description{
  Utilities for converting files in R documentation (Rd) format to other
  formats or create indices from them, and for converting documentation
  in other formats to Rd format.
}
\usage{
\special{R CMD Rdconv [options] file}
\special{R CMD Rd2pdf [options] files}
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
  \command{R CMD Rdconv} converts Rd format to plain text, HTML or LaTeX
  formats: it can also extract the examples.

  \command{R CMD Rd2pdf} is the user-level program for producing PDF output
  from Rd sources.  It will make use of the environment variables
#ifdef unix
  \env{R_PAPERSIZE} (set by \command{R CMD}, with a default set when \R
  was installed: values for \env{R_PAPERSIZE} are \code{a4},
  \code{letter}, \code{legal} and \code{executive})
#endif
#ifdef windows
  \env{R_PAPERSIZE} (default \code{a4}, also \code{legal} and
  \code{executive})
#endif
  and \env{R_PDFVIEWER} (the PDF previewer).  Also,
  \env{RD2PDF_INPUTENC} can be set to \code{inputenx} to make use of the
  LaTeX package of that name rather than \code{inputenc}: this might be
  needed for better support of the UTF-8 encoding.
  
  \command{R CMD Rd2pdf} calls \code{tools::\link{texi2pdf}} to produce
  its PDF file: see its help for the possibilities for the
  \command{texi2dvi} command which that function uses (and which can be
  overridden by setting environment variable \env{R_TEXI2DVICMD}).
  
  Use \command{R CMD \var{foo} --help} to obtain usage information on utility
  \code{\var{foo}}.
}
\seealso{
  The chapter \dQuote{Processing Rd format} in the \dQuote{Writing \R
    Extensions} manual.
}
\keyword{utilities}
