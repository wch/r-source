\name{RdUtils}
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
#ifdef unix
\special{R CMD Rd2dvi [options] files}
#endif
#ifdef windows
\special{R CMD Rd2dvi.sh [options] files}
#endif
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
  plain text, HTML, LaTeX, S version 3 (Sd), and S version 4 (.sgml)
  formats are supported.  It can also extract the examples for
  run-time testing.

  \code{Rd2dvi} and \code{Rd2txt} are user-level programs
  for producing DVI/PDF output or pretty text output from Rd sources.

  \code{Sd2Rd} converts S (version 3 or 4) documentation formats to Rd
  format.

  Use
  \code{R CMD foo --help}
  to obtain usage information on utility \code{foo}.  
}
\note{
  Conversion to S version 3/4 formats is rough: there are some
  \code{.Rd} constructs for which there is no natural analogue.
  They are intended as a starting point for hand-tuning.
}
\seealso{
  The chapter \dQuote{Processing Rd format} in \dQuote{Writing \R
    Extensions}
#ifdef unix
  (see the \file{doc/manual} subdirectory of the \R source tree).
#endif
#ifdef windows
  (see the Manuals sub-menu of the Help menu on the console).
#endif
}
\keyword{utilities}
